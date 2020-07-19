# unzufall

> Anyone who considers arithmetical methods of producing random digits is, of course, in a state of sin.
>
> *J. v. Neumann*

The `unzufall` plugin provides a high level interface to generating pseudo-random data for MDAL modules.


## Graphical Interface

To generate data with `unzufall`, you must have an active selection in the current module view. Data can be generated for trigger, note, key, and numeric block fields.

**Data PRNG**: The pseudo-random number generator used to generate data. Refer to the documentation of the [prng](../prng/README.md) plugin for a detailed description of the available generators.

**Data Variance**: The range of permitted output values, as a fraction of the total range of values that the field is allowed to assume. Must be a value between 0 and 1. Has no effect on trigger and non-note key fields.

**Data Mean**: The mid point of the variance range. For example, if a field can take the values 0 - 100, then setting both *Data Variance* and *Data Mean* to 0.5 will yield values between 25 and 75.

**Position PRNG**: The pseudo-random number generator used to randomize distances between data values.

**Position Density**: Specifies how many values to generate on average. A value of 0.5 will yield a new data value every 2 rows on average. A higher value means more values. Use a value of 1.0 to generate new data values on every row.

**Position 1-bit Mode**: When enabled, positions are derived from a bitstream. This is useful when using a LFSR-based *Position PRNG* with a short period, such as `sid-noise` or `tia-noise`. *Position Density* is ignored in 1-bit mode.

**Position Sync**: Randomize positions only once, and use the same positions for all selected columns.

**Paste Mode**: Specify how the generated values will be pasted into the selection. *Replace* performs a regular paste, *Porous/under* will only be paste at empty rows in the current data, and *Porous/over* will retain the current data at empty rows in the new data.

The graphical plugin interface is available in the `Generate` menu.

## Keybinding Actions

`unzufall`: Opens the graphical interface.


## Trivia

The name "Unzufall" is a mash-up of the German words "Unfall" and "Zufall", meaning "accident" and "randomness, coincidence", respectively.
