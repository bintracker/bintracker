(module md-audio
    (make-audio
     audio-channels
     audio-rate
     audio-data
     raw->audio
     import-raw
     import-pcm-wav
     audio->raw
     audio->pcm-wav
     export-pcm-wav
     normalize
     resample)

  (import scheme (chicken base) (chicken bitwise)
	  srfi-1 srfi-4 bitstring typed-records)

  ;; ---------------------------------------------------------------------------
  ;; ## math utils
  ;; ---------------------------------------------------------------------------

  (define-constant pi 3.1415926535)

  (define (sinc x)
    (if (zero? x)
	1
	(/ (sin (* pi x))
	   (* pi x))))

  ;; convert unsigned int represented in an arbitrary number of bits to its
  ;; signed counterpart
  (define (unsigned->signed val bits)
    (let ((posint-max (sub1 (expt 2 (sub1 bits)))))
      (if (> val posint-max)
	  (- (+ 1 (bitwise-and posint-max (bitwise-not val))))
	  val)))

  ;; ---------------------------------------------------------------------------
  ;; ## u8vector utils
  ;; ---------------------------------------------------------------------------

  ;;; Read an unsigned byte from a u8vector. Synonym for u8vector-ref
  (define (read-u8 u8v offset)
    (u8vector-ref u8v offset))

  ;;; Read a signed byte from a u8vector.
  (define (read-s8 u8v offset)
    (unsigned->signed (u8vector-ref u8v offset) #x7f))

  ;;; Read an unsigned word from a u8vector.
  (define (read-u16 u8v offset)
    (+ (u8vector-ref u8v offset)
       (* #x100 (u8vector-ref u8v (+ 1 offset)))))

  ;;; Read an unsigned dword from a u8vector.
  (define (read-u32 u8v offset)
    (+ (read-u16 u8v offset)
       (* #x10000 (read-u16 u8v (+ 2 offset)))))


  ;; ---------------------------------------------------------------------------
  ;;; ## Types
  ;; ---------------------------------------------------------------------------

  ;;; The main data structure for wrapping audio data.
  ;;; Internally, all audio data is handled as float.
  (defstruct audio
    ((channels 1) : fixnum)
    ((rate 44100) : fixnum)
    ((data '()) : (list-of vector)))


  ;; ---------------------------------------------------------------------------
  ;;; PCM WAV file handling
  ;; ---------------------------------------------------------------------------

  (bitpacket pcm-wav-header
	     ("RIFF")
	     (chunk-size 32 little)
	     ("WAVEfmt ")
	     (subchunk1-size 32 little)
	     (1 16 little)
	     (num-channels 16 little)
	     (sample-rate 32 little)
	     (byte-rate 32 little)
	     (block-align 16 little)
	     (bits-per-sample 16 little)
	     ("data")
	     (subchunk2-size 32 little))

  ;;; Generate a function that extracts the sample at {{offset}} in an u8vector
  ;;; {{data}} representing raw PCM data
  (define (make-sample-extractor rate sample-length signedness byte-order)
    (let* ((unsigned (eq? signedness 'unsigned))
	   (bits-per-sample (* 8 sample-length))
	   (maxint+1 (expt 2 (if unsigned
				 bits-per-sample (sub1 bits-per-sample))))
	   (sample->float (lambda (n)
			    (exact->inexact (/ n maxint+1))))
	   (read-operation (case sample-length
			     ((1) read-u8)
			     ((2) read-u16)
			     (else (error "unsupported bit depth"))))
	   (transform-operation (if unsigned
				    identity
				    (lambda (x) (unsigned->signed
						 x (* 8 sample-length))))))
      (lambda (data offset)
	(sample->float (transform-operation (read-operation data offset))))))

  ;;; Convert raw (headerless) PCM WAV data into an `audio` structure.
  ;;; SAMPLE-LENGTH is the number of bytes used by one sample in one channel.
  (define (raw->audio data channels rate sample-length signedness byte-order)
    (letrec* ((extract-sample (make-sample-extractor rate sample-length
						     signedness byte-order))
	      (extract-channel
	       (lambda (channel remaining-data)
		 (if (= 0 (u8vector-length remaining-data))
		     '()
		     (cons (extract-sample remaining-data
					   (* channel sample-length))
			   (extract-channel
			    channel
			    (subu8vector remaining-data
					 (* sample-length channels)
					 (u8vector-length remaining-data))))))))
      (make-audio channels: channels rate: rate
		  data: (map (lambda (ch)
			       (list->vector (extract-channel ch data)))
			     (iota channels)))))

  ;;; Construct an `audio` structure from a raw (headerless) PCM audio file.
  (define (import-raw filename
		      #!optional (channels 1) (rate 44100) (sample-length 1)
		      (signedness 'unsigned) (byte-order 'little-endian))
    (raw->audio (with-input-from-file filename read-u8vector)
		channels rate sample-length signedness byte-order))

  ;;; Construct an `audio` structure from a PCM WAVE (RIFF) file.
  (define (import-pcm-wav filename)
    (let ((data (with-input-from-file filename read-u8vector)))
      (bitmatch data
		(((pcm-wav-header bitpacket)
		  (check (= chunk-size (- (u8vector-length data) 8)))
		  (sample-data bitstring))
		 (raw->audio (bitstring->u8vector sample-data)
			     num-channels sample-rate
			     (quotient block-align num-channels)
			     'signed 'little-endian))
		(else (error "Not a valid PCM WAV file.")))))

  (define (audio->raw audio bits-per-sample signedness byte-order)
    (letrec* ((maxint (sub1 (expt 2 (sub1 bits-per-sample))))
	      (float->list-of-int
	       (lambda (f)
		 (let ((i (if (eqv? 'unsigned signedness)
			      (inexact->exact (round (+ maxint (* f maxint))))
			      (inexact->exact (round (* f maxint))))))
		   ;; TODO variable bit depth
		   (if (= bits-per-sample 8)
		       (list i)
		       (if (eqv? 'little-endian byte-order)
			   (list (bitwise-and #xff i)
				 (bitwise-and #xff (quotient i #x100)))
			   (list (bitwise-and #xff (quotient i #x100))
				 (bitwise-and #xff i)))))))
	      (transform-data
	       (lambda (remaining-data)
		 (if (= 0 (vector-length (car remaining-data)))
		     '()
		     (append (flatten
			      (map (lambda (v)
				     (float->list-of-int (vector-ref v 0)))
				   remaining-data))
			     (transform-data
			      (map (lambda (v) (subvector v 1))
				   remaining-data)))))))
      (list->vector (transform-data (audio-data audio)))))

  (define (construct-pcm-wav-header audio bits-per-sample)
    (let ((subchunk2-size (* (vector-length (car (audio-data audio)))
			     (audio-channels audio)
			     (quotient bits-per-sample 8))))
      (bitconstruct
       ;; ChunkID
       ("RIFF" bitstring)
       ;; ChunkSize
       ((+ 36 subchunk2-size) 32 little)
       ;; Format + Subchunk1ID
       ("WAVEfmt " bitstring)
       ;; Subchunk1Size (always #x10 for PCM)
       (#x10 32 little)
       ;; AudioFormat = PCM
       (1 16 little)
       ;; NumChannels
       ((audio-channels audio) 16 little)
       ;; SampleRate
       ((audio-rate audio) 32 little)
       ;; ByteRate = SampleRate * NumChannels * BitsPerSample/8
       ((* (audio-channels audio)
	   (audio-rate audio)
	   (quotient bits-per-sample 8))
	32 little)
       ;; BlockAlign = NumChannels * BitsPerSample/8
       ((* (audio-channels audio) (quotient bits-per-sample 8)) 16 little)
       ;; BitsPerSample
       (bits-per-sample 16 little)
       ;; Subchunk2ID
       ("data" bitstring)
       ;; Subchunk2Size = NumSamples * NumChannels * BitsPerSample/8
       (subchunk2-size 32 little))))

  ;;; Extract raw PCM audio from the audio structure AUDIO.
  (define (audio->pcm-wav audio bits-per-sample)
    (audio->raw audio
		bits-per-sample
		(if (= 8 bits-per-sample) 'unsigned 'signed)
		'little-endian))

  ;;; Export the `audio` structure AUDIO to the PCM WAV file FILENAME, using a
  ;;; bit depth of BITS-PER-SAMPLE.
  (define (export-pcm-wav filename audio bits-per-sample)
    (with-output-to-file filename
      (lambda ()
	(write-u8vector
	 (bitstring->u8vector
	  (bitstring-append (construct-pcm-wav-header audio bits-per-sample)
			    (vector->bitstring
			     (audio->pcm-wav audio bits-per-sample))))))))

  ;;; Normalize the range of the `audio` structure AUDIO to (-1.0, 1.0)
  (define (normalize audio)
    (let* ((dlists (map vector->list (audio-data audio)))
	   (mmin (apply min (concatenate dlists)))
	   (mmax (apply max (concatenate dlists))))
      (make-audio
       channels: (audio-channels audio)
       rate: (audio-rate audio)
       data: (map (lambda (channel)
		    (list->vector
		     (map (lambda (x)
			    (if (= mmin mmax)
				(cond ((< x -1.0) mmin)
				      ((> x 1.0) mmax)
				      (else x))
				(+ (/ (* 2 (- x mmin))
				      (- mmax mmin))
				   -1.0)))
			  channel)))
		  dlists))))

  ;;; Resample AUDIO to the sample rate TARGET-RATE using Blackman windowed
  ;;; interpolation. Optionally, WINDOW-SIZE may specify the number of samples
  ;;; used by the Blackman window function for interpolation.
  (define (resample audio target-rate #!optional (window-size 44))
    (let* ((step (/ (audio-rate audio) target-rate))
	   (m1 (min 1 (/ target-rate (audio-rate audio))))
	   (minrate (min target-rate (audio-rate audio)))
	   (window-offset (/ (sub1 window-size) 2))
	   (blackman (lambda (n len)
		       (- 0.42
			  (+ (* 0.5 (cos (/ (* 2 pi n) (sub1 len))))
			     (* 0.08 (cos (/ (* 4 pi n) (sub1 len)))))))))
      (make-audio
       channels: (audio-channels audio)
       rate: target-rate
       data:
       (map
	(lambda (channel)
	  (let ((vlen (vector-length channel)))
	    (list->vector
	     (map
	      (lambda (j)
		(let ((klow (inexact->exact (ceiling (- j window-offset))))
		      (khigh (inexact->exact (floor (+ j window-offset)))))
		  (* m1
		     (apply + (map
			       (lambda (k)
				 (* (sinc (* (/ minrate (audio-rate audio))
					     (- k j)))
				    (blackman (- k (+ j window-offset))
					      window-size)
				    (cond
				     ((< k 0) 0)
				     ((>= k vlen) (vector-ref channel
							      (sub1 vlen)))
				     (else (vector-ref channel k)))))
			       (iota (+ 1 (- khigh klow)) klow))))))
	      (iota (inexact->exact (ceiling (/ vlen step)))
		    0 step)))))
	(audio-data audio)))))

  ) ;; end module sample-utils
