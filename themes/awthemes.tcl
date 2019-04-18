#!/usr/bin/tclsh
#
# awdark/awlight.tcl
#
# Copyright 2018 Brad Lanam Walnut Creek, CA
#
# zlib/libpng license
#
# Helper routines:
#
#   ::ttk::theme::awdark::setMenuColors .menuwidget
#     Sets the menu colors and also changes any checkbutton and
#     radiobutton types to use thematic images.
#     Side effect: The menu will have -hidemargin set to true.
#
#   ::ttk::theme::awdark::setTextColors .textwidget ?-dark?
#     Sets the text widget colors.  If -dark is specified, the
#     background will be set to the darker color (like TEntry).
#
#   ::ttk::theme::awdark::setListboxColors .listboxwidget
#     Sets the listbox widget colors.
#
#   ::ttk::theme::awdark::setBackground color
#     requires the colorutils package
#
#   ::ttk::theme::awdark::setHighlight color
#     requires the colorutils package
#
# Mac OS X notes:
#   To style the scrollbars, use:
#        ttk::scrollbar .sb -style Vertical.TScrollbar
#     or ttk::scrollbar .sb -style Horizontal.TScrollbar
#   This will turn off the aqua styling and use the theme styling.
#   Also note that the styling for the scrollbar cannot be configured
#     afterwards, it must be configured when the scrollbar is created.
#
# 2.3
#   - Added padding for Menu.TCheckbutton and Menu.TRadiobutton styles
# 2.2
#   - Added support for flexmenu.
#   - Fixed listbox frame.
# 2.1
#   - Added Menu.TCheckbutton and Menu.TRadiobutton styles
#     to support menu creation.
# 2.0
#   - Add setBackground(), setHighlight() routines.
#     If wanted, these require the colorutils package.
#   - Removed the 'option add' statements and use a bind
#     to set the combobox's listbox colors.
#   - Merge awdark and awlight themes into a single package.
#   - More color cleanup.
#   - Make notebook top bar use dynamic colors.
# 1.4
#   - change button anchor to default to center to follow majority of themes
# 1.3
#   - clean up images a little.
# 1.2
#   - fix button press color
# 1.1
#   - per request. remove leading :: for the package name.
# 1.0
#   - more fixes for mac os x.
# 0.9
#   - changes to make it work on mac os x
# 0.8
#   - set disabled field background color for entry, spinbox, combobox.
#   - set disabled border color
# 0.7
#   - add option to text helper routine to set the darker background color.
#   - fix listbox helper routine
# 0.6
#   - fixed hover color over notebook tabs
# 0.5
#   - menubutton styling
#   - added hover images for radio/check buttons
# 0.4
#   - paned window styling
#   - clean up disabled widget color issues.
# 0.3
#   - set disabled arrowcolors
#   - fix namespace
#   - fix disabled check/radio button colors
#   - set color of scale grip and scrollbar grip to dark highlight.
# 0.2
#   - reduced height of widgets
#   - labelframe styling
#   - treeview styling
#   - remove extra outline color on notebook tabs
#   - fix non-selected color of notebook tabs
#   - added setMenuColors helper routine
#   - cleaned up the radiobutton image
#   - resized checkbutton and radiobutton images.
# 0.1
#   - initial coding
#

package require Tk

try {
  set ap [file dirname [info script]]
  if { $ap ni $::auto_path } {
    lappend ::auto_path $ap
  }
  unset ap
  package require colorutils
} on error {err res} {
}

proc awinit { } {
  global awthemename

  foreach awthemename {awdark awlight} {
    namespace eval ::ttk::theme::$awthemename {
      variable colors
      variable images
      variable imgdata
      variable vars

      proc init { } {
        variable colors
        variable vars

        set vars(theme.name) $::awthemename
        set vars(cache.menu) [dict create]
        set vars(cache.text) [dict create]
        set vars(cache.listbox) [dict create]
        set vars(cache.popdown) false
        set vars(nb.img.width) 20
        set vars(nb.img.height) 3

        _setThemeBaseColors

        foreach {k} [array names colors base.*] {
          regsub {^base} $k curr nk
          set colors($nk) $colors($k)
        }
        foreach {k} [array names colors text.*] {
          regsub {^text} $k curr nk
          set colors($nk) $colors($k)
        }
        foreach {k} [array names colors highlight.*] {
          regsub {^highlight} $k curr nk
          set colors($nk) $colors($k)
        }

        _setImageData
        _createTheme
        _setStyledColors
      }

      proc _setThemeBaseColors { } {
        variable vars
        variable colors

        if { $vars(theme.name) eq "awdark" } {
          array set colors {
              base.disabledfg #919282
              base.disabledbg #2d3234
              base.disabledborder #202425
              base.frame      #33393b
              base.dark       #252a2c
              base.darker     #1b1f20
              base.darkest    #000000
              base.bpress     #424a4d
              base.lighter    #525c5f
              base.lightest   #ffffff
              highlight.selectbg   #215d9c
              highlight.darkhighlight #1a497c
              highlight.selectfg   #ffffff
              }
          set colors(base.arrow) $colors(base.lightest)
          set colors(base.arrow.disabled) $colors(base.lighter)
          set colors(base.border) $colors(base.darkest)
          set colors(base.border.light) $colors(base.darkest)
          set colors(text.text) $colors(base.lightest)
          set colors(base.tabborder) $colors(base.darkest)
          set colors(base.tabinactive) $colors(base.frame)
          set colors(base.tabhighlight) #8b9ca1
          set colors(base.entrybg) $colors(base.darker)
        }
        if { $vars(theme.name) eq "awlight" } {
          array set colors {
              base.frame      #e8e8e7
              base.disabledfg #8e8e8f
              base.disabledbg #cacaca
              base.disabledborder #c0c0bd
              base.dark       #cacaca
              base.darker     #8b8391
              base.darkest    #000000
              base.bpress     #e8e8e7
              base.lighter    #f0f0f0
              base.lightest   #ffffff
              highlight.selectbg   #1a497c
              highlight.darkhighlight #1a497c
              highlight.selectfg   #ffffff
              }
          set colors(base.arrow) $colors(base.darkest)
          set colors(base.arrow.disabled) $colors(base.darker)
          set colors(base.border) $colors(base.dark)
          set colors(base.border.light) $colors(base.dark)
          set colors(text.text) $colors(base.darkest)
          set colors(base.tabborder) $colors(base.frame)
          set colors(base.tabinactive) $colors(base.darker)
          set colors(base.tabhighlight) $colors(base.darkest)
          set colors(base.entrybg) $colors(base.lightest)
        }
      }

      proc _setImageData { } {
        variable vars
        variable imgdata

        if { $vars(theme.name) eq "awdark" } {
          # cb-sa
          set imgdata(cb-sa) {
             iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABHNCSVQICAgIfAhkiAAAAAlwSFlz
             AAAOJgAADiYBou8l/AAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAABSdEVY
             dENvcHlyaWdodABDQyBBdHRyaWJ1dGlvbi1TaGFyZUFsaWtlIGh0dHA6Ly9jcmVhdGl2ZWNvbW1v
             bnMub3JnL2xpY2Vuc2VzL2J5LXNhLzQuMC/DVGIFAAAB2klEQVQ4jYWSy2sTURSHv3tnmojTNJ2J
             1KWaqnXj+wUKQgmtgv+CoLgSilIX0kh0ERAfWKFRfJGFRSpUiKKoK0EEQfFdqjufaZJS6aSlLjIV
             MnNdiCPBZuYs77nfd3/ncAUgTdM8LqTsQ6nFgCK4BEL8EnCjWq2e0E3TTMfi5kCya01MaloI+6c8
             16X4+dNhpdQizTCM0a51G5ZoIbCu6+zpSdGb6mZlZ5LS1I+WqUp5te4pZYTBlmmSzQyQXLEcgPzw
             CFJKNE0Telhcy2xn8HSWpR0dADx7/oL7Dx/5fRkERyMRspm0D3+fKJG7ch2l/u05UHBw/z4/ds2Z
             5+yFIRxnvuGOL4hGIuzauYPtWzcDsG3LJvbu7gFAKcVg7hKlcuW/R/wdnEwfY+P6tczN/eRoMUN/
             3yGEEACMFu7y8vXbBVP6gmikBYB4vI3c+TPEWlsBGBv/yK3bhaZj+iN8LU74h3/hWs1h6PK1hqU1
             FbxaIGJ++CbTtt0UbhC8H/9AZXLSb7x5N8bjJ08DYQBdCOF4nhcHuHg1T2+qm5nZWe7cexAYXSmF
             67qIRCJxqs1K9C/rXGVIGfgtGuDyty9O1Z4eEYDeblnnBBzQNd1DhAvq9bompCzM2PaR3/NDnfex
             Az+KAAAAAElFTkSuQmCC
          }
          # cb-sd
          set imgdata(cb-sd) {
             iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABHNCSVQICAgIfAhkiAAAAAlwSFlz
             AAAOJgAADiYBou8l/AAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAABSdEVY
             dENvcHlyaWdodABDQyBBdHRyaWJ1dGlvbi1TaGFyZUFsaWtlIGh0dHA6Ly9jcmVhdGl2ZWNvbW1v
             bnMub3JnL2xpY2Vuc2VzL2J5LXNhLzQuMC/DVGIFAAABsUlEQVQ4jY2S204TURSGv7F7NmWAbVrk
             gkjwUAFjI9cl4J0hNhpfwMQbfQVNfAZ9BhMvfAvjIRCEYExDEIWEQ22T0gg17XQOnWZme9EwkQBt
             1+X61/evQ5aRzWalGwSvwXhigEkfoaEN+r0l5QvhBu03whx4lkwmrX7gk/B977kbBBg3pqaPhkfU
             aC9ACMGd29dJpxQN2+F7YZum3TgWaBK9YKWGeHA/h1JDACytFE6khOgFjwxbPMovYA0OAPBrp8j2
             zu9Yv9QNltIkvzgXw3+O/vJ1bfNUTVeD+dzdeGzPb/Hh0zphGJ5vIKXJzPQk1ybHAZjKTJC5OQGA
             1pqPn7/hOP6ZJvEN8os5xq6kcD2fet1mLjcbF62t/6ByeHzulP+tYABgDSZ5/PAe0ux4HxxU2Nza
             u3DN2KBWa8RJKTsP6Xktllc3LoRPGRTLh2fEldUNfL/Vn0GpVKVhO7Gwu1dmv1jpCgMIDCLoXPrL
             UoFbmas0HY+tn/s9YSBKpEfT46HWs6YwTcfxKJWrVKs1oijqSnq+5xKFb0VKqZe1uh012+2nBsh+
             2moI0LxLX1av/gGvIJ2+zm/8QQAAAABJRU5ErkJggg==
          }
          # cb-sn
          set imgdata(cb-sn) {
             iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABHNCSVQICAgIfAhkiAAAAAlwSFlz
             AAAOJgAADiYBou8l/AAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAABSdEVY
             dENvcHlyaWdodABDQyBBdHRyaWJ1dGlvbi1TaGFyZUFsaWtlIGh0dHA6Ly9jcmVhdGl2ZWNvbW1v
             bnMub3JnL2xpY2Vuc2VzL2J5LXNhLzQuMC/DVGIFAAAB00lEQVQ4jYWSzUtUYRTGf+edrzvleLs6
             AzGuzf6CKMhqVW0iIiJoEbUONILACWtAbBE55ghFjQ1SDEUQRLSKWvRhafYXpINlIEoD3hpnrtfN
             fVuINyVn7lme8/6e85yHVwBlmmZGlLqE1jsATfMSDWtKZNy27X4xTfNaNGb0me3JhIgEsOultUfV
             th3XXR0Ty7IW2nen00FwJBLh1MkT7Ons5FelwoOxIpXFheWw1npnEJxKpbibz7G3qwuA28N5RAQl
             SsJBdpPJJI+LBTo60gC8fvOW0pOn60MB1Qw2jBj3Rod9eLY8R3bgJlr/y7mpwJXeHt92ve5wtS+D
             4zhb3vgChhHj+LGjHDnUDcDh7oOcPXMaAM/zyPTfYO77j/+W+Bnkc0Mc2L+PZdvm3PmLDGSvsxFu
             oTjOuw8ft3XpCxhGFIA2y+JZ6RGmaQIw9eUr9wsPG57pn/Btpuw3N+BarU52YBDP84IF3m9jcejO
             CItLSw3hLQJT09PMz//0BxOfJnnx8lVTGCAUj8d74y2JFq1hZrZMSCkmPk+SGxnFdd0mqKZWrbpi
             WdZgNGZcTlhtgV96M7zy5/fqmuOUBAi3tu66JYoLItI4rc241iEl8ty27Z6/RA+bmP+MCq8AAAAA
             SUVORK5CYII=
          }
          # cb-ua
          set imgdata(cb-ua) {
             iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABHNCSVQICAgIfAhkiAAAAAlwSFlz
             AAAOJgAADiYBou8l/AAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAABSdEVY
             dENvcHlyaWdodABDQyBBdHRyaWJ1dGlvbi1TaGFyZUFsaWtlIGh0dHA6Ly9jcmVhdGl2ZWNvbW1v
             bnMub3JnL2xpY2Vuc2VzL2J5LXNhLzQuMC/DVGIFAAAAw0lEQVQ4je3MsU4CURCF4XNmNoSs2eDs
             dpY0Wthp4uvYG22BhBfgKWwtfA8SO2s1xgQ6lvZisvcOBQkFBez2/P33E4CY2YQiT3DPATiOR5D/
             BF7rup5mZjYuBjYaXt8UonrC7kox4u/769nd+6yqanl7/3ClLfF+khI+P+ZrSe4XXTEAiAhUldJZ
             Ho7Og/MAgJAMKaXO0N0RY4TmeW6bTbgbWNkj2Rovfn9CCOGNALLLspwReMw0S2jxaJpGKfK+Xq1e
             tqAEP9PwDWwUAAAAAElFTkSuQmCC
          }
          # cb-ud
          set imgdata(cb-ud) {
             iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABHNCSVQICAgIfAhkiAAAAAlwSFlz
             AAAOJgAADiYBou8l/AAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAABSdEVY
             dENvcHlyaWdodABDQyBBdHRyaWJ1dGlvbi1TaGFyZUFsaWtlIGh0dHA6Ly9jcmVhdGl2ZWNvbW1v
             bnMub3JnL2xpY2Vuc2VzL2J5LXNhLzQuMC/DVGIFAAAArUlEQVQ4je3OvQqCYBjF8XNeTbBEImiI
             JgPtttoa24WmuoDWJrsuh9oLovyON32bGiqwbPa//87zEIA28bylqjgHqj5I1KYUAHGhUNt9GK7o
             uNO1pmsL0+xa9fK1PM+S8l5u6LjusWfZQ367/PGIQppEJwHQaIoBgCQIGqKxfKsdaAeeA/IvqRQU
             IAXAXVFkaVNf3PIUYKAPbMs/X2PGMpoJoPMLrgBJMDiMR/4DiN82UH/SIRYAAAAASUVORK5CYII=
          }
          # cb-un
          set imgdata(cb-un) {
             iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABHNCSVQICAgIfAhkiAAAAAlwSFlz
             AAAOJgAADiYBou8l/AAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAABSdEVY
             dENvcHlyaWdodABDQyBBdHRyaWJ1dGlvbi1TaGFyZUFsaWtlIGh0dHA6Ly9jcmVhdGl2ZWNvbW1v
             bnMub3JnL2xpY2Vuc2VzL2J5LXNhLzQuMC/DVGIFAAAAuElEQVQ4je2TvQ7BYBiFz3krpYRPwyC9
             DTdkl5gRN+AaDFaD22oMEp+fRCvlew0GkYi2sYlnf57pHAIQY8yEIkOo1gEoPkMFLkIurbUzGmOm
             frU2Np1uk2SO+0DV4WjtOU2TBcMwjDu9KCoqPyOK7Sbeiao2ysoAQBJCoZQ2XyrAdwH8A78SIJmo
             5v3nHQrnHLwgCMJblvX9WuAXn7TidNgn7npdEUCl1WrPKRiQdIV0VU/ItbV2dAeqdT3jjotV3gAA
             AABJRU5ErkJggg==
          }

          # cb-sn-small
          set imgdata(cb-sn-small) {
             iVBORw0KGgoAAAANSUhEUgAAAAwAAAAMCAYAAABWdVznAAAABHNCSVQICAgIfAhkiAAAAAlwSFlz
             AAAKnQAACp0BJpU99gAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAABSdEVY
             dENvcHlyaWdodABDQyBBdHRyaWJ1dGlvbi1TaGFyZUFsaWtlIGh0dHA6Ly9jcmVhdGl2ZWNvbW1v
             bnMub3JnL2xpY2Vuc2VzL2J5LXNhLzQuMC/DVGIFAAABT0lEQVQokXWRzUqCURCGn3NUjqmL9COk
             RSWmQWQGrYooupqgZRC0LaT/ougGgihpWVEiEt5BUNYiNSgolWgRiflT6mmVEH7N6mXmmZeXGeFy
             uQZtSh1JYTFAS8xLa938QOsZ4TaMK6PLOyqkOetwOOjr7SGXL/CUzWSklBbPf/DE+BiJi1OiB/v4
             fT6k1ea0ohH/wbvbGyil2Nje4TqVQkgwtR4IBlpwLJ7gMHrcmll/RWhoiNpXlefnF9aWIyilSGey
             LEWW/5hZAaanJtnb2SJ1e8d9OkOg30+xWGRufoFKtdq+UC6XAQgPhwgPhwBYjKzyksu1xZUAN7d3
             VCqVVjMWT3CZTJpeTiLQtVqNk7NzAPL5Aivrm6YwTRBuj+fa8HaPdHTYCQaCPD49Uip9trFaa97f
             Xh+E0+kOK7s8slhsnaBNf4IUuvld/2w06rM/bvF1KxpyPTUAAAAASUVORK5CYII=
          }
          # cb-un-small
          set imgdata(cb-un-small) {
             iVBORw0KGgoAAAANSUhEUgAAAAwAAAAMCAYAAABWdVznAAAABHNCSVQICAgIfAhkiAAAAAlwSFlz
             AAAKnQAACp0BJpU99gAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAABSdEVY
             dENvcHlyaWdodABDQyBBdHRyaWJ1dGlvbi1TaGFyZUFsaWtlIGh0dHA6Ly9jcmVhdGl2ZWNvbW1v
             bnMub3JnL2xpY2Vuc2VzL2J5LXNhLzQuMC/DVGIFAAAAi0lEQVQokd2RsQ3CMBRE75+JgmQXBAtl
             BhoKhkJisIzDAmwADUqBKGL7qKBAQabm1e9dcxZC2DZtO9BcBETMI6mMkA7WxXiKm35v/Oa+C9yu
             lzNJt67JAGBm4KLxhGBV+xURqE9/8B+BQT/bBWDJaZTqjSTkPD3M+27XLjk416wAzX9CU5nSPed0
             fALSuy0PFYJR0AAAAABJRU5ErkJggg==
          }
          # rb-sn-small
          set imgdata(rb-sn-small) {
             iVBORw0KGgoAAAANSUhEUgAAAAwAAAAMCAYAAABWdVznAAAABHNCSVQICAgIfAhkiAAAAAlwSFlz
             AAAKnQAACp0BJpU99gAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAABSdEVY
             dENvcHlyaWdodABDQyBBdHRyaWJ1dGlvbi1TaGFyZUFsaWtlIGh0dHA6Ly9jcmVhdGl2ZWNvbW1v
             bnMub3JnL2xpY2Vuc2VzL2J5LXNhLzQuMC/DVGIFAAABbElEQVQokXWSsW4TURBFz7wH73nt9QJK
             YgslMU4KoM0HEEoKOgT5FGip+RMi8QEpCR8AJVBAZBykWDZC3niXt7BvKLJRbAlufTRz594RrtRy
             zr0QMc+MNV2AWMdcNR5WVfUK+AUgDXzHeX+03usPO93MLw1hkc/DdHL2tQrhETASIHHef9gcDO9a
             30a3HqDJGgBSzjDjY/6EktPRyacqhD3rWq2XG/3bj30ns/H+AXpjB1wGLkM7PfTmLvbnZ9x1m4Uy
             iBF40k5TF7f30WR9yeWFY0020K2HtNOuE8NTY61NgcbGMnwlbd8CwFjbMQr6T2pFzSAFE+t6ASDF
             j//AihQzAGKsF0bhTXGeVzJ+ixQTVhcqUk6R8TGLfB40xsPLWN9vDob3rvmEuL2Pti5ipZhiT9/x
             O5R8H518rELYu7xy4Lw/Wuv1d9KV4pTzPA+zydmXprhvy7F459xzEXNgrMkAYh3nqvF18xoVwF8b
             KZBY5e2lGwAAAABJRU5ErkJggg==
          }
          # rb-un-small
          set imgdata(rb-un-small) {
             iVBORw0KGgoAAAANSUhEUgAAAAwAAAAMCAYAAABWdVznAAAABHNCSVQICAgIfAhkiAAAAAlwSFlz
             AAAKnQAACp0BJpU99gAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAABSdEVY
             dENvcHlyaWdodABDQyBBdHRyaWJ1dGlvbi1TaGFyZUFsaWtlIGh0dHA6Ly9jcmVhdGl2ZWNvbW1v
             bnMub3JnL2xpY2Vuc2VzL2J5LXNhLzQuMC/DVGIFAAABG0lEQVQokY2SsU4CURBF787M7oPAroVB
             Y0iQSmu+whg7o36Kttb+iSQ2dn6FlhoLCQmaYDRhYcG3++Zhg3ELCNz6zJ3JvRPgXxURuWKWc2KK
             AcCrH6u6rnPuBsAPAAQLeD8y5mG7sdOuJ1umZIJJOrJfn8O33NojAP0AQDUy5qnZah9IGGKZXFFg
             0O+95NZ2WKLourG7d1KpVnkpDYCYEYZhMpvOAmKi01ocR6vgP9XiJGKhM2Lm+jq4tKlGc2C+6QDm
             AHnVbFPee81Ivb/Lxmm+Dp6kI6vOdf9ifWy22oerYi2KAu/93nNubYcBOFW9n2aTYxaJjalI+ehx
             mtrhx+B1Udx3UDIyInLJLBfElACAV5+qutvFa+QA8AsEQHDkbXq4nAAAAABJRU5ErkJggg==
          }

          # cb-sn-pad
          set imgdata(menu-cb-sn-pad) {
             iVBORw0KGgoAAAANSUhEUgAAAA4AAAAMCAYAAABSgIzaAAAABHNCSVQICAgIfAhkiAAAAAlwSFlz
             AAAKnQAACp0BJpU99gAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAABSdEVY
             dENvcHlyaWdodABDQyBBdHRyaWJ1dGlvbi1TaGFyZUFsaWtlIGh0dHA6Ly9jcmVhdGl2ZWNvbW1v
             bnMub3JnL2xpY2Vuc2VzL2J5LXNhLzQuMC/DVGIFAAABZUlEQVQokXXRv0rDUBTH8e9NrmgxYGuQ
             4NBuOkj9W3wA8TV8CgcHURQRFPw/CoKLICIipdTBBxBFrK2LU6tmsAqpVmiMxabXQVoH0zMdLr8P
             9xyOMAxjoK29/UATuglKI7BEva5857tanapUKvcAImKaGbPHGhVasDGMTmLRKLZt81jIZ95LpQSA
             pml6pBWanJjgPJ1if28Xy7L4neq3JArRCq2tLiOlZH5xiXzhAbS/bOBXg/F4Ex0eHZNMpf9lZKMZ
             Gozjui7Fl1dWlheRUpLN5Vjb3A5cQzbG2lpf5fomw/NzkVg0iuM4TM/MUqvVWkPP+wRgPDEGCVBK
             MbewhOM4gai5423ujq+vavPx5DTJxeVVS9SEnueRSp8B8GTbbGzvBKfrqEYrIt3dWdPqHQ6FOujv
             6yNfKOC6n/+MUorSazH3/vY2AiDC4fCIpusHut7WBSrwpiCU739/1H1/qlwuZwF+AHh0hdcmxafz
             AAAAAElFTkSuQmCC
          }
          # cb-un-pad
          set imgdata(menu-cb-un-pad) {
             iVBORw0KGgoAAAANSUhEUgAAAA4AAAAMCAYAAABSgIzaAAAABHNCSVQICAgIfAhkiAAAAAlwSFlz
             AAAKnQAACp0BJpU99gAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAABSdEVY
             dENvcHlyaWdodABDQyBBdHRyaWJ1dGlvbi1TaGFyZUFsaWtlIGh0dHA6Ly9jcmVhdGl2ZWNvbW1v
             bnMub3JnL2xpY2Vuc2VzL2J5LXNhLzQuMC/DVGIFAAAAnklEQVQoke2RzQqCYBBF78yXuBFCPqJ3
             aBe+Tgsfzudx2yv0Zxm4EXVuCzMhMqF1ZzVzuQcGRqIo2gRhmKk4D1DxETFjd2nqeldV1R4AJPY+
             96v1VnTCeUIzFOdjfiuKBABU1cVzEgCIKvqrehSEzFpjW8bxR/7iOwa+RLO2JPmtDgAgCbO2HPYF
             zdLr6ZA5FywBTvxU2HXNnWbpkDwA1ec6OAmodXwAAAAASUVORK5CYII=
          }
          # rb-sn-pad
          set imgdata(menu-rb-sn-pad) {
             iVBORw0KGgoAAAANSUhEUgAAAA4AAAAMCAYAAABSgIzaAAAABHNCSVQICAgIfAhkiAAAAAlwSFlz
             AAAKnQAACp0BJpU99gAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAABSdEVY
             dENvcHlyaWdodABDQyBBdHRyaWJ1dGlvbi1TaGFyZUFsaWtlIGh0dHA6Ly9jcmVhdGl2ZWNvbW1v
             bnMub3JnL2xpY2Vuc2VzL2J5LXNhLzQuMC/DVGIFAAABlUlEQVQokXWRsW/TQBjFf+dL7mI3dkox
             qQitqoqhC0NZu3SElQHYWJhYWNn5C9hZETsrDEgVEgMSIxJIKKraplACauva+JLYH4ObxEXqWz7p
             3vvdu9OnmKtljHmmPO+h1noBoCiKTODNKM+fA1ktizqfPWPtu7i7fHMhjGw9kKVn4+GPw75z7i7Q
             r4NNY+3n3urarWYroFzZRoKrlZX/Qe/tMHEpg93+V+fcbeAvgLbWPl2Ku/f9sNMsNh4gi+tgIjAh
             BF2ks07j+BvNRiNyLveKyeQ9gCfwqB1FfnljC/G7tddXEj+mWNkmaLeNgnvTc09rHSqlEP/a/8xc
             /pUq7OlwBorM7r6EutA/C3mllCciAunPS2CBbAhAWZTJvLGQl8nJcaoHH1Hp4UVYBJUdofd3OEtO
             c5Hy9dRSgDbGfLq+urZpWr4qelvQXq6sbIg++MA4zxjs7X4ZVesYTUGA2Bj7dimON9qdxUApdV4o
             pMlp/vvX0feRc3eAg3rjVLpp7RMPHntaR4AqC0mgfOWcewGM6z//B5ZqmsLd48LOAAAAAElFTkSu
             QmCC
          }
          # rb-un-pad
          set imgdata(menu-rb-un-pad) {
             iVBORw0KGgoAAAANSUhEUgAAAA4AAAAMCAYAAABSgIzaAAAABHNCSVQICAgIfAhkiAAAAAlwSFlz
             AAAKnQAACp0BJpU99gAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAABSdEVY
             dENvcHlyaWdodABDQyBBdHRyaWJ1dGlvbi1TaGFyZUFsaWtlIGh0dHA6Ly9jcmVhdGl2ZWNvbW1v
             bnMub3JnL2xpY2Vuc2VzL2J5LXNhLzQuMC/DVGIFAAABR0lEQVQokY2RvU7DMBRGP9uxUwhpJH4q
             UanqwMoAD4FgZUBsLEwsrOw8ATsrYmNgZuABkBiROlUVUlWQELRpmtrxjRlo2gwFehbb0nd8fa8Z
             ZlQ8z7sQnncshAgAgIhGlOf31phLAKNSFmyy1pXvP6xt1LZWqpFfDiTDOPt467W11gcA2mVRKt9/
             rjea21IpzMNai26n3dJa7wJIAUBIKc9X12tHy0Eg51oAOOfwpKqm45TnRI8AwBnnJ2EULf0mFQRh
             qATnh9PLhBAhY+wvp1RZhNO9cws5E2Zpnru87xa0c8rjqWgNXQ++PpP/pHjQHxPZ2+LMAAil1NNm
             o7mjfH9us5kx6L52XszPdxgAEAAcEd2lSbLHGYtUpSKLYTnnMBz0x++9bstovQ/gq1yxQHApzyTn
             p1yIKgCWk4uJshtr7RWArPyKb8UjfFtWcf+KAAAAAElFTkSuQmCC
          }

          # rb-sa
          set imgdata(rb-sa) {
             iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABHNCSVQICAgIfAhkiAAAAAlwSFlz
             AAAOJgAADiYBou8l/AAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAABSdEVY
             dENvcHlyaWdodABDQyBBdHRyaWJ1dGlvbi1TaGFyZUFsaWtlIGh0dHA6Ly9jcmVhdGl2ZWNvbW1v
             bnMub3JnL2xpY2Vuc2VzL2J5LXNhLzQuMC/DVGIFAAACcUlEQVQ4jYWSzW+UVRSHn3ve9947M/2g
             9Gu0mpSgGARbSBd+sJDEvYmJ7Pkv/Bt0wRICiRt3bgzBLQvjxg2BqAuQ2k6HEUvp1JaZTjtz33nv
             PS6QhlCQszqL3/NbPOfA4Vlwzn3nvd+0zvWcc7vWuR3v/XXg0xfD5rndOueu5dZ+Of/OibHp2bqx
             zgEQY2R7q01zZblTDAa3QggXgM7zBbn1/qfZ+htL753+oGaMoEApNTCCTXugCkCrsVK01hoPixCW
             gI4B8N5fnpqpXzy5eGYEDDu19+n5twABQFGq5RbTvd8wGmmtrYYHjdVfhiF8lgHHc2uvnP3w4zGM
             8Hj8HHtujiSeZHKSyVFjKeQI+/5NxkKLiYmJvL2xPhXL8laWO/fVsePvnp+YnJRu9QQ9/zZqssNq
             DUTjKLMRRopH+ErVb7fbdclFvpicmckBdv38y+GDEsPATqNkHJ2aJqX4icSUZqvVGmoy0v/B/40i
             DLNRRASRzAgHfvW1MIAaPcgqimQij/r7exhNiJavLRBN2NgjxogmTZJS+uGfzcdDgPFBA9HhK2Gj
             iWqxiSGxvdVGxPwsRVF8+1dzrR/LkvFBk8pwC6PxMEzCxS5T+7+jqqwt3++EEL7OgG5m7ejukydL
             9bk5N1qsY4iUUsOQyLREUsFYaDHTu40h0Vi+3+/sbN+MMV569srivP/xyNHJ86fOnB3NshyAZHJA
             EC2eSlNl9Y97/Y31hytFCB8B/Wd30xjj92VZjv/dbC4aYzLrnNg8Q0gM+n02N9b17q93drvdzo0i
             hM+BwatczTvnvvGVyp/Wuo61tusrlQfe+6vAwovhfwHxTg80BiJS7QAAAABJRU5ErkJggg==
          }
          # rb-sd
          set imgdata(rb-sd) {
             iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABHNCSVQICAgIfAhkiAAAAAlwSFlz
             AAAOJgAADiYBou8l/AAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAABSdEVY
             dENvcHlyaWdodABDQyBBdHRyaWJ1dGlvbi1TaGFyZUFsaWtlIGh0dHA6Ly9jcmVhdGl2ZWNvbW1v
             bnMub3JnL2xpY2Vuc2VzL2J5LXNhLzQuMC/DVGIFAAACQElEQVQ4jYWTQUtUURiGn++ce+ZeZ8bU
             tIUl6UQyCxdtKkIoXLVpYcsg2gT9g/oDLSMIcteiH9CqVQvbCO4KWwSSaKRBJQY6Os7kOTP33q+F
             Do5l+a4OfO/7cPh4P+EPjVarFcn0gQi3VXUYQETWVXlNZF6sLS2tdful620vjI8/VeR+HMdFG7nI
             yP44VyVL22kI4ZegL7+srDwEskPA1FQ09m191jl7JU56yiIdrkEFRHMAVJXg9xrtdvZ+bWT4JnNz
             qQWoFAozhcjdSnqKZTEGX67Q7L+EL10glMYIxfNgDC7dIYpcQfP8TN/2zvD21uYbqVSrVXLelXt7
             TyFCfeAyqRsAY48uRzNca4ve2gLkOY1Go47hqj09OPi4EMeT1kbiyxdpJWf/DgOIQW0CgGvXQHB5
             llqDMu2cMwC+eA49Ltz5hFhCcQSAyDmDMm1y6BcxqIlR+Xe4G6Imxoghh35zOBBET8wjuu/tyAAN
             VUUyj3AyQVBMHlBVBJpGlNksbasALvyE/0IU5zdgv1iKMmskZyb4UAco7nzCtndBj4GoYtu7FOtL
             AATv65Lz3NZqmz/6hoauoToWRTaK976jtge1DlQRzTDaohA2KNc+IOQE732q2dvVz8vPIoC9JL6L
             9wvAaBwnSWnnI9QNmS0BYLMmHNQ5BO9baevrXpLc29/JgSYmJsrNdvrKwGScJH3WRnRuQlXJspTg
             fV1V54sFd2dxcbFxBNBRZXz8BsgjVK8jB8VQzRCZR3iyurw83+3/DbK6ACz9O82sAAAAAElFTkSu
             QmCC
          }
          # rb-sn
          set imgdata(rb-sn) {
             iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABHNCSVQICAgIfAhkiAAAAAlwSFlz
             AAAOJgAADiYBou8l/AAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAABSdEVY
             dENvcHlyaWdodABDQyBBdHRyaWJ1dGlvbi1TaGFyZUFsaWtlIGh0dHA6Ly9jcmVhdGl2ZWNvbW1v
             bnMub3JnL2xpY2Vuc2VzL2J5LXNhLzQuMC/DVGIFAAACT0lEQVQ4jYWSzWtUZxTGf+e8572TzEdM
             MolGAxVq0UWx0ICICAruhYL+Lf4NuuiiG7HQTXfdFNFVQVC7dtFFoV0otGJEnMlkMnNnesd775zX
             RYyIH/XA+eBwnoeHhwMfxukY489m1jOziZnlZjY0y24DF94/lnfmGGP8UUSvrK6vd5rtjoQQAHB3
             iumU4aA/qsryUV3XV4HRuwRmZg/aS4e21o5sNEUEREhZByQgr0aQHIDhYFCOdne2q6raAkYGEGL8
             odXufLu+cbQJgm+ex1dPIrKvICVHJtuEp/dZ6XYzETaHO/3bdV1fCsCXFuzmsS+Od0SV+ckrpJWv
             wBYhZG+yAQtrpOUT6OBvFhYaNsnzLik9CmZ2baXbvbjYbKlvnCGtngKNH1orArZAaiyje0+IFhvT
             6eSIiup3zXbHAHzt64+D35Ioqb0Jaiy2WuB+TpP74ZhFUEPkf8AHoYHUWEFEEBFRIJF409LnCfZt
             fVtVVV+UVQk+R7z6LFS8RmZD3B1ScnX3X//L8wpAen/AvPw02h3G/0KaU0ynoPq71nX9095wt3B3
             tP8nkj+DjylJc2TWJzx7CMBgpzeqy/J6AMZq1i6LYqu9tJTp8DGkChrLkGpkXkJdoIO/CP/8BmnO
             oN8rZtPpPXf//uCV1czuLjZbFw8f22yr6v42ZCAB6mJfRErs9ntFPtp7UlXVWaAIBwLd/RdPaWm8
             O/gmQQgWNCjgNXVVMcnH6eXz7fzVrLhTVdVlYPYpq46b2Y0syx6b2cjMxjHLnoYYbwGn3z9+DdZ0
             +7udtE82AAAAAElFTkSuQmCC
          }
          # rb-ua
          set imgdata(rb-ua) {
             iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABHNCSVQICAgIfAhkiAAAAAlwSFlz
             AAAOJgAADiYBou8l/AAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAABSdEVY
             dENvcHlyaWdodABDQyBBdHRyaWJ1dGlvbi1TaGFyZUFsaWtlIGh0dHA6Ly9jcmVhdGl2ZWNvbW1v
             bnMub3JnL2xpY2Vuc2VzL2J5LXNhLzQuMC/DVGIFAAAB0klEQVQ4jZWSu24TQRSG/zm7c2bXEZAY
             XxAFgQQiJGKQQgVIUNBAgYQEJc/BM0CdBiSadDQI8QIBKgokLk0gxtgmFwh2cFg7sJ7NzA5FhGQF
             G2W/6kjnfL/+4gD/UmHmBaVUSzJvM3NPMm8ppZ4CuLz3WAzMkpkf+lLempw+daBQKgvJDACw1qKz
             2UazVo2Sfv+11vo2gGgwwJdKPS+Vj8zNnJnNCUFDiu2yUq8lK436WqL1HIDIAwCl1HyhWL52unJu
             TAgxUgaAQxN5DwK5Xq97KbV2gQBMQYg7M7OVsf+aAxw7Ma3CMDzv+/5Vz2e+e3zq5JXxfH507yGo
             IFSddrtMPtHNfLHoZ5EBYOJwAWlqL5BN01IY5rL6ICIQeYIAOJdZ38XBgTyib/HvX5llay1c6lJK
             0/TJj9b3nawBnc02iMRLSpLk0WqzEVtj9i0759CoLkda63sEYA1CzC+9f7u934B6dTlOEr0I4JUH
             ANaYF8bsXIx+bh0tlEpMNPwlnHP4/PFDvLG+Wku0vg7AeH931trHxpiD683mWSGEJ5nJlxJCCPTj
             GK2Nr27p3Ztetxs9S7S+AaA/quEkM99XQfBJSo6klF0VBF+UUg8AVPYe/wF4NLEd4MjCaAAAAABJ
             RU5ErkJggg==
          }
          # rb-ud
          set imgdata(rb-ud) {
             iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABHNCSVQICAgIfAhkiAAAAAlwSFlz
             AAAOJgAADiYBou8l/AAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAABSdEVY
             dENvcHlyaWdodABDQyBBdHRyaWJ1dGlvbi1TaGFyZUFsaWtlIGh0dHA6Ly9jcmVhdGl2ZWNvbW1v
             bnMub3JnL2xpY2Vuc2VzL2J5LXNhLzQuMC/DVGIFAAABsUlEQVQ4jZWTMWsUURSFz7nzdmZYzWpS
             pFBQdotgo2C20iptCkGif8HGWv+DnYhYCP4AGxErO402FiksbIQMIy7LWq1x183OvHlv3rXQjUtA
             4pzqwj3f4cDlEsfU6126rBLugdhWoC2ABlUvwl1Vffhlf//9sp+Lod/vt8bT2VOB3orTdMWYFsnf
             awVQewdblhNV3WPtb+d5PvkbsLVluqPRW2PizTRN28dbLauqbGWtHUrtN/M8nwgAdIejR8aYqyfB
             ABDHSZzEyflgWi8BgBc2NnpG8fHU6ZUOyJP4I80PZ1Pnwo5IwJ04SdtNYACIk6QTRXpfSN6MTGQa
             0QBMZADFNVHqOilNeYAESAoU2pw+SoEQ/KaheYYCgIYgqvrCeeeaBtTeAZR3YhCeucoW0GYtbFlO
             gtYPJMuyoQgfF0Ux+2/YlkWAvvmaZR8iADgYj3fPrK5er4M/12rF8b9ABWCLsvC+yvx8vj2dTn20
             2P34Pn5+dm2tU1l7hcKIf25LEhoCvHdazg9/htq/UuduDAaDElj6xoW63e5FFXOX5A6g6yAJ6AEh
             r1HzSZ5//rTs/wUo28NldkL2cAAAAABJRU5ErkJggg==
          }
          # rb-un
          set imgdata(rb-un) {
             iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABHNCSVQICAgIfAhkiAAAAAlwSFlz
             AAAOJgAADiYBou8l/AAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAABSdEVY
             dENvcHlyaWdodABDQyBBdHRyaWJ1dGlvbi1TaGFyZUFsaWtlIGh0dHA6Ly9jcmVhdGl2ZWNvbW1v
             bnMub3JnL2xpY2Vuc2VzL2J5LXNhLzQuMC/DVGIFAAABqUlEQVQ4jZWTu24TURCGv3PmHEd4bUMc
             mzhQINEHiVBBAQUVBRISlDwHzwAlogGJho4GRbwAl4oijwANkrnZ8WXXS9bZPRcKhGQFJcr+1Ugz
             36cpZuD/bFtrX4nISERyEVmIyMyYxi5w8+iwWqmttfYFqPvdfr+dtDtKRAAIIXCQ50wn49RX1Z5z
             7gGQrgqMMeZ9q3N2p7c5aCqlOC7T/f1yPhkPvfc7QKoBxNqnSbtztT/YOhEG6PZ6jfVe/6IxZhdA
             A5dV5GF/sJWcSK5kfaO3JqZxDbgtxphH57obt84kiT6tAMAYs1Ysi02NUveSdsfUgQGarRbR++s6
             hnDeNmxdHqUUSimlgUiszQMQAa21/lFWZW04hAAxBu29f/M7y6q6goM8R4l81CGEl/PZtAgh1BJM
             xr9SV5aPNTCM8Gz0/Vt+enhUBO/fAZ8EIIbwIXh/47AoLjRb7cZx1xhjZDoeFdl89sU5dwdw8q8X
             QnjtvO9k08mVGKOIMfrvMylcVbHI0vhzOFyUh8u3zrm7wPK4DS8ZY55Yaz+LSCoimVj7Vax9Dmwf
             Hf4Dx8Kr4mNF2+sAAAAASUVORK5CYII=
          }
        }

        if { $vars(theme.name) eq "awlight" } {
          # cb-sa
          set imgdata(cb-sa) {
             iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABHNCSVQICAgIfAhkiAAAAAlwSFlz
             AAAOJgAADiYBou8l/AAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAABSdEVY
             dENvcHlyaWdodABDQyBBdHRyaWJ1dGlvbi1TaGFyZUFsaWtlIGh0dHA6Ly9jcmVhdGl2ZWNvbW1v
             bnMub3JnL2xpY2Vuc2VzL2J5LXNhLzQuMC/DVGIFAAAB3klEQVQ4jYWSvW9SURiHn3O4VMtH1ARb
             DJsNCXU25oZEVAbYcCaBtJONbWwhDGricPkDKJMdbIxhEId2IDGxCUxEBwY1MdXusggJKVPD1Qsc
             B9KbYOXyjuf3Pr/36wjDMOS1xevPhXRtoZQHKRROMVYC+I0Qb07POi+0q56lZ8Hg0tPogzt+t9vt
             yJ7HaDSi9enLE/WTy5pLuLai9+fDlmVRrb7j5OQHweANdna2PQft91lNqbHXveAMdzodstksx8ff
             ATAMAyklmpRCm9dut9sllXpIu90GIJVKsbHxaCJKkE6waZpkMhkbjkQilMu7CCHsHEcDwyjabft8
             Pvb3X+H1eqdybAPTNKnVatTrdQAajQaVSmWSJCV7ey8Jh8MXitg7WFtbp9lsEggEODr6QC6XR6nJ
             l8jncyQSif92aRuY5gCAXq9HIpGk3+8DEIvFKBQKM8e0R1hdvWU/nsN+v59yeRcpZ6/KVpLJ5AWx
             WDQIhUIz4SmDWOwuKys3bSEej5NOpx1hAE0IMRgOh1c0TaNUKlGtVlleDrK5+Xjq3v+GUoqhNUIb
             j9Xr1sevuei9215d19F1fW5VpRSfW98GQohDrT/oGrTV4sHbX+sLbm08lwasP5YLIQ5Pzy5t/wXP
             QKK9o+YbTwAAAABJRU5ErkJggg==
          }
          # cb-sd
          set imgdata(cb-sd) {
             iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABHNCSVQICAgIfAhkiAAAAAlwSFlz
             AAAOJgAADiYBou8l/AAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAABSdEVY
             dENvcHlyaWdodABDQyBBdHRyaWJ1dGlvbi1TaGFyZUFsaWtlIGh0dHA6Ly9jcmVhdGl2ZWNvbW1v
             bnMub3JnL2xpY2Vuc2VzL2J5LXNhLzQuMC/DVGIFAAABrklEQVQ4jY2TT08TQRyGn1lW2FkLpHSt
             VMWmBDQNJiRASqgGDvopJPGi8Rto4mfALyEJdw8mSCKagHrgwK3GCPInthezi0JJp9ltdjygkzRI
             u3Obed/nnXd+yYhKpdKrlFoCFoUQl0iwtNYRsCKlfGYrpV5KKR8PDaVdIUQSHoAgOHqilMIGHiaB
             W60W375859fRb1L9KSanJ9xqtbZoCyF6usH14zrv1z5yclwHYG5+5p/UY3erenpyytqbD6hGE4Dx
             4ihjt0eNbnWCwzBi/e2mgb1shlJ5qs3TMWDr07ap7UiHhQdlLKsdMbswjNj5usePgxoAezuH7O8e
             npksi/n7c7iX5blLzAzWVzfwfwZIVzKYHmDr87YxTc1OcjV35b8tTQMdxwCohmL19TuiMAIgX7hB
             8c74hc80AelM2hyGf2FH9lG6N30h3BYwks+dE2fvzuA4fckCro3k6B9IGaEwludm4XpHGMDWWsdw
             NunyQon93QPclEtx4lZXGIhtIcSy7wdPPS/jZoc9ssNeEhDfDxpCiGVba/282WzG1WrtEdDb7V9o
             rQFC4BXw4g8dNY0KDGeioAAAAABJRU5ErkJggg==
          }
          # cb-sn
          set imgdata(cb-sn) {
             iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABHNCSVQICAgIfAhkiAAAAAlwSFlz
             AAAOJgAADiYBou8l/AAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAABSdEVY
             dENvcHlyaWdodABDQyBBdHRyaWJ1dGlvbi1TaGFyZUFsaWtlIGh0dHA6Ly9jcmVhdGl2ZWNvbW1v
             bnMub3JnL2xpY2Vuc2VzL2J5LXNhLzQuMC/DVGIFAAABzklEQVQ4jY2Sv4oaURTGv3PvQNwZEiMb
             xUokSlgICUwzhaCVrQ8gGBHRaklwREiCUwhTWPsAIT7AWoidXVBYbLZLiCYhqSxWQa3kYmZuCtkB
             2XXMKe93ft/5d6nVarHgo/OPRPxSQqogSPiFBAEQRPR5vb21lCeBZx+ehoLvL14nH3POfdm7cF0X
             06+/3rq3MqBwYpcXr07Du90Og8EAs9kMkUgEpVJJvV7evFFcKTWu+MOLxQL1eh3T6RQAUKvVwBgD
             JyLlVLvL5RLVahXz+RwAkM1mkc/n9yIBzA8WQsA0TQ9OJBKwLAtE5OX4GnQ6Ha9tVVXRbrehqupB
             jmcghMBwOMRoNAIAjMdj9Hq9fRJjsG0b8Xj8XhFvB41GA5PJBKFQCN1uF7ZtQ8r9lyiXy0in0w92
             6RkIIQAAq9UKxWIRm80GAGAYBiqVytExvRGSyaT3eAdrmgbLssDY8VV5SiaTuSeapoloNHoUPjAw
             DAOxWMwTUqkUcrmcLwwACiPaOo4T5Jyj2Wyi3+8jHA6jUCgc3PuhcBwXiuvKTz++/a69ePlc03Ud
             uq6frAoAP7//2RKxK2UjFi0s5Nn1l3WJc+b+D+z+dTiIXa23gXf/AJaPnECURofgAAAAAElFTkSu
             QmCC
          }
          # cb-ua
          set imgdata(cb-ua) {
             iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABHNCSVQICAgIfAhkiAAAAAlwSFlz
             AAAOJgAADiYBou8l/AAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAABSdEVY
             dENvcHlyaWdodABDQyBBdHRyaWJ1dGlvbi1TaGFyZUFsaWtlIGh0dHA6Ly9jcmVhdGl2ZWNvbW1v
             bnMub3JnL2xpY2Vuc2VzL2J5LXNhLzQuMC/DVGIFAAAAzUlEQVQ4je3MsWrCUBiG4e/7T47ViNAO
             ihcigkJBL0Rwl3ZtCw65keLkpHehQwYVegntoi46haBJzu/k4CIJ3Urf/XkZBIE8VeofFDOCqg+h
             4l5OCeAEcnKIdmPv0W+8N5uNt26/XbPW3rXXsixDuFy/6DfKYmhG3V5+DADGGHSeWz6Igai6qi3l
             x9dEBJ4IpbC8uQC/G+B/8FcGJOM0TQtDVUWaZBDn9DNcbCLnXCG8Cr9iknPvGO8D/GhlNt0OS9bL
             dUnOiQE5P0QPrxeAzEWdFLFAUQAAAABJRU5ErkJggg==
          }
          # cb-ud
          set imgdata(cb-ud) {
             iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABHNCSVQICAgIfAhkiAAAAAlwSFlz
             AAAOJgAADiYBou8l/AAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAABSdEVY
             dENvcHlyaWdodABDQyBBdHRyaWJ1dGlvbi1TaGFyZUFsaWtlIGh0dHA6Ly9jcmVhdGl2ZWNvbW1v
             bnMub3JnL2xpY2Vuc2VzL2J5LXNhLzQuMC/DVGIFAAAAqElEQVQ4je3OMQ6CQBSE4ZmHkpBQGPYA
             hptZSWlPYqUHsLXCmqtwCGK0XbRYAgmRZ2NhrFhrpv/+DMuyDNJ0vSeDTFVXmDCST9XXua6vB1ZV
             dYyiaGdMEpOc4qGqsLZxfd+fhOTWB38ewJgkBpAJidAHf0cAhOItfzYH5gAAiCqGf6CqAsAgAC7W
             Nq1voGkeraoWCwB513W83e4bEVlOweM4DiQL51z+BsK+Poco4SryAAAAAElFTkSuQmCC
          }
          # cb-un
          set imgdata(cb-un) {
             iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABHNCSVQICAgIfAhkiAAAAAlwSFlz
             AAAOJgAADiYBou8l/AAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAABSdEVY
             dENvcHlyaWdodABDQyBBdHRyaWJ1dGlvbi1TaGFyZUFsaWtlIGh0dHA6Ly9jcmVhdGl2ZWNvbW1v
             bnMub3JnL2xpY2Vuc2VzL2J5LXNhLzQuMC/DVGIFAAAAwklEQVQ4je3MMWoCYRRF4XvfG8ioGCNx
             BwoKojsKpJekjUKK2Yi4ATdh5QqUBA2x1mYsZBDyv2clWMmoXfD032GSJFJ5eB6Q2nN4EYTjXA4C
             2JMcpdn6M3qMa/2nauWj1W2UVfWsPWZm+J79vNnaY1FKr9XJjwFARNBs14sUvoi5lzTKj08nSlIu
             lqcRuG2A++C/DITMQghX4RAMYubDxfx3Z2YX4eXXKiNlHG33mwQbL0wn6auq5LrYX1BQxmkWvx8A
             z/Q/zoRhgI8AAAAASUVORK5CYII=
          }

          # cb-sn-small
          set imgdata(cb-sn-small) {
             iVBORw0KGgoAAAANSUhEUgAAAAwAAAAMCAYAAABWdVznAAAABHNCSVQICAgIfAhkiAAAAAlwSFlz
             AAAKnQAACp0BJpU99gAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAABSdEVY
             dENvcHlyaWdodABDQyBBdHRyaWJ1dGlvbi1TaGFyZUFsaWtlIGh0dHA6Ly9jcmVhdGl2ZWNvbW1v
             bnMub3JnL2xpY2Vuc2VzL2J5LXNhLzQuMC/DVGIFAAABUUlEQVQokXWRv2rCcBSFv1/wX+JQfkRx
             UEGFiCKFIAWdM7m7mSC1+C4ugoOPYOkjKAQfIFCkIKWIi6BTBocWhAREuwnF9I73fudwOFckk0mj
             XC6/FQoFmUgkBBFzPp+vh8Phe7/fP4tarfa+WCyepJRRLKfTid1uh67rdLvdT0XX9cx/sOu6NBoN
             LMvC932KxeJDTAgRGWO5XNLv9wmCgMlkgmmaKIoiYlHwer3Gtm3CMMRxHAaDwe12E6xWKzRNo1Qq
             MRwOCcOQZrPJeDz+YxYDmM/n9Ho92u021WqV7XZLNptlNpuRSqXuBZqmAeB5Hp7nATCdTsnn83dx
             FYBWq4Wqqrelbdt0Op3I5hQhxFVVVRzHAaBSqTAajSLhy+WCqNfrH67rmvF4nM1mg2EYpNPpOzgI
             AizL+hJSysdcLveayWQkEPkTIcT1eDz++L7/8gv+E2ey4F2//wAAAABJRU5ErkJggg==
          }
          # cb-un-small
          set imgdata(cb-un-small) {
             iVBORw0KGgoAAAANSUhEUgAAAAwAAAAMCAYAAABWdVznAAAABHNCSVQICAgIfAhkiAAAAAlwSFlz
             AAAKnQAACp0BJpU99gAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAABSdEVY
             dENvcHlyaWdodABDQyBBdHRyaWJ1dGlvbi1TaGFyZUFsaWtlIGh0dHA6Ly9jcmVhdGl2ZWNvbW1v
             bnMub3JnL2xpY2Vuc2VzL2J5LXNhLzQuMC/DVGIFAAAAo0lEQVQokd3RMQqDQBCF4XlT7DZCGHbB
             RguLgE26nGHPEXK2kCt4iLQ5gVsJColaWCxrOqsFrfPX34OBgdb6XFXVsygKUUqBEoUQVu/9t23b
             G+q6fjVNcxWRlN2a55mcc282xtg9TESUZRmVZXliAMkzUjEz+CjeRv8wALAexTFG4r7vP9M07eJl
             Wch7P0JELnmeP6y1QkTJnwBYh2EYu667/wAhkDBFom/zLwAAAABJRU5ErkJggg==
          }
          # rb-sn-small
          set imgdata(rb-sn-small) {
             iVBORw0KGgoAAAANSUhEUgAAAAwAAAAMCAYAAABWdVznAAAABHNCSVQICAgIfAhkiAAAAAlwSFlz
             AAAKnQAACp0BJpU99gAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAABSdEVY
             dENvcHlyaWdodABDQyBBdHRyaWJ1dGlvbi1TaGFyZUFsaWtlIGh0dHA6Ly9jcmVhdGl2ZWNvbW1v
             bnMub3JnL2xpY2Vuc2VzL2J5LXNhLzQuMC/DVGIFAAAB00lEQVQokW2RPWgTYRjH/+97rUe+elFP
             jUFeL3CUxg9E6aiFFBwMZHDRgF/oVJwUydC5bkFBwSFZdDEEg4NLQKmgg0axIBRaqk0IxnBee4nv
             Nc1dQ+D6Omhqh/zW58f/eR7+BP8ZjUajd/1+/9VwODwmSRI4567ruq+azeZ9AM4uF4c0TVvM5XJb
             7XZb2LYtbNsWnHNRLBb7uq5/l2V5HAAIgBFN0xZKpdIpdjSGB8/fY6VhwfO2oUX3Yvb6NNzuBpLJ
             ZK1arZ6WIpHI7Uwmc/nsuanRG3NFzC/UYLQ6MH9vYuWHhXdfa7h0fhInjsfHKpVKiAaDwZvpdNr/
             +MUHLNXWIYTYfSpWf7Yx9/QtEonESCAQuEBDoZAiyzKW62vwxDaG0fjFAQCqqvqo+BdJKRkqAwDI
             35kQQlDHcXiv18NJ/TAkSRrq60f2AwAsy3IlSqnw+XzTM9cu7vm81MA67+78QQhBXDuIh3dSmH/z
             ul8ul58RAJQx9qlQKExOxI+RJy8/YnHVgOcJjLMDuHdlCtaaiVQq9a1er58ZbN3HGPuSzWY3TdPc
             Ka7Vaol8Pr8Vi8WWAbBBcQOoqqq3FEWZURRFIYSg0+l0HccpGIbxCEAfAP4AsTDDpD5SGF4AAAAA
             SUVORK5CYII=
          }
          # rb-un-small
          set imgdata(rb-un-small) {
             iVBORw0KGgoAAAANSUhEUgAAAAwAAAAMCAYAAABWdVznAAAABHNCSVQICAgIfAhkiAAAAAlwSFlz
             AAAKnQAACp0BJpU99gAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAABSdEVY
             dENvcHlyaWdodABDQyBBdHRyaWJ1dGlvbi1TaGFyZUFsaWtlIGh0dHA6Ly9jcmVhdGl2ZWNvbW1v
             bnMub3JnL2xpY2Vuc2VzL2J5LXNhLzQuMC/DVGIFAAABOklEQVQokY3QsWrCUBQG4D9JJTQhXimB
             isMlgWsfoFtWX6JLp07SrYNv0C1TR7O4Fcd2yRNEhIZCKVJK2lFEULnBeqM49HTSSkHqD2f7/nPg
             aPhNqVar3ViWdVmpVMqGYUBKWRRF8TgcDm8BqB2LU8/zXtvt9nI2m1Ge55TnOUkpqdvtroUQmWma
             Zxt85HneS5qmW/h3siwjIcQnAMeoVqvXrVbrotFolLAntm2jXq+X+/2+owkhnnu93rlpmvv8NkEQ
             fOiO47BDMAC4rnusExEdpAEQEelKKblarQ4qTCaTQpdSRp1OZ/EfjuN4rZR6AACdc/6UJMn3vrcO
             BgPyff8dgLVZcMI5T8Mw/BqPx1s4nU4piqKl7/tvADgAaDtXddd1rxhjTcYY0zQN8/l8oZS6H41G
             dwDWAPADBOS0dhP9s/YAAAAASUVORK5CYII=
          }

          # cb-sn-pad
          set imgdata(menu-cb-sn-pad) {
             iVBORw0KGgoAAAANSUhEUgAAAA4AAAAMCAYAAABSgIzaAAAABHNCSVQICAgIfAhkiAAAAAlwSFlz
             AAAKnQAACp0BJpU99gAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAABSdEVY
             dENvcHlyaWdodABDQyBBdHRyaWJ1dGlvbi1TaGFyZUFsaWtlIGh0dHA6Ly9jcmVhdGl2ZWNvbW1v
             bnMub3JnL2xpY2Vuc2VzL2J5LXNhLzQuMC/DVGIFAAABUklEQVQokXWRsYrCQBCGvxUDopF4aBGE
             aGFjUl2ZwsrOR1AUwSJPovgGqS3uDSxtFOz0jiuE64QQsDpDDpJq4faaSxrjwsDO8H8z8zNC13XH
             sqy3ZrPZBEoUv9/7/f4dhuE0SZIvAGzb/giCQMVxXBhhGKr9fq8ul4uybfs961RqtVovhmEUjtlu
             tziOw2g0IkkS/rcCoAyIZ9BisUBKie/79Pt9hBC5ttDT+XzOIc/zmEwmD5py9jmdTtTrdSzLwvM8
             pJS4rstqtSq0Uc7Wms1mDAYDOp0O1+sV0zTZbDZomvYcrNVqAByPRwCEEPi+j2mahVDu0XVdKpVK
             XpzP5wyHw6cQQEkpRbVaZTweA9Dr9Vgul4VipZTKwSiK4jRNWa/X7HY7DocDuq4/QGmaEkVRnOWi
             0Wi8ttvtt263a2iaVnhTKaUKguDndrtN4zj+BPgDk/mTpWV8DuoAAAAASUVORK5CYII=
          }
          # cb-un-pad
          set imgdata(menu-cb-un-pad) {
             iVBORw0KGgoAAAANSUhEUgAAAA4AAAAMCAYAAABSgIzaAAAABHNCSVQICAgIfAhkiAAAAAlwSFlz
             AAAKnQAACp0BJpU99gAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAABSdEVY
             dENvcHlyaWdodABDQyBBdHRyaWJ1dGlvbi1TaGFyZUFsaWtlIGh0dHA6Ly9jcmVhdGl2ZWNvbW1v
             bnMub3JnL2xpY2Vuc2VzL2J5LXNhLzQuMC/DVGIFAAAAqElEQVQokeWRsQqDQAyG/xPchFjOSRDn
             69QH8vF8nZZunYPgVI8UvOmg6VBtO2jt3g8yHPxfLiEmy7J9VVWttdYCSLDMfRiGa9d1zTiOFwCA
             c+7EzCoiX4uZ1Tl3nDslRVHsiGjlozdEhGmqpwjAbFoTxphXdm2nTf5CVNWfw/oRTrz3EkLYlEII
             8N7L/DZ5nh/KsmzruqY0TRdvGmNUZr71fd+IyBkAHkinWoV3DyNgAAAAAElFTkSuQmCC
          }
          # rb-sn-pad
          set imgdata(menu-rb-sn-pad) {
             iVBORw0KGgoAAAANSUhEUgAAAA4AAAAMCAYAAABSgIzaAAAABHNCSVQICAgIfAhkiAAAAAlwSFlz
             AAAKnQAACp0BJpU99gAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAABSdEVY
             dENvcHlyaWdodABDQyBBdHRyaWJ1dGlvbi1TaGFyZUFsaWtlIGh0dHA6Ly9jcmVhdGl2ZWNvbW1v
             bnMub3JnL2xpY2Vuc2VzL2J5LXNhLzQuMC/DVGIFAAAB1klEQVQokW2RzWvTcBjHv7/UFSVZKKXM
             vqC/NHQIOsRdVBREj+ZUD0FE6cWTl10E7VVPO8ZbwUuH1lKEFv+CiuKhCPWgU4sS66jdSLulaV4W
             JuTnQZplbF944OF5ns/zwkOwr+PZbPaxIAi3k8kkDwCmaXqu674ZDAZPAXg4QllJktar1apvmiab
             TCah1ev1PVmWewDyUYAAmMvn891ms7mUyZ3C6os2fmyMwBiDnEuhXLoBx9qBoig9XdeXAewCQCyT
             yayUy2X10uUrc6Undbzt6tjctrG14+Bb38C7Tzru3LwIWaJip9PhbNtuAwDH83xJVdUTWuM91n8Z
             YIwduKG3McLqWhuKosR5nr81i3OiKM7H43F8/z06BM2kD7dBCEEikZgPwVkxR8iR0P8cBwAIgiDs
             zNm2bfm+j3PyScQ47hBECMHi6RSCIIBlWXYITqfT57VazV1Rr+J8IY1jsf3JHCE4Ky3g0b3raLVa
             vud5r6LviFFKPzYajQuFwiJ59voDPv/cAgsCnKELeHj3Gjb/DFAsFr/2+/1lAHvRjVKU0q6maa5h
             GOHzx+Mxq1Qqu5TSLwByB06I+LF0Ov1AEIT7oiiKjDHiOI7tuu7L4XCoAfgbBf8B1trLnD3kvsEA
             AAAASUVORK5CYII=
          }
          # rb-un-pad
          set imgdata(menu-rb-un-pad) {
             iVBORw0KGgoAAAANSUhEUgAAAA4AAAAMCAYAAABSgIzaAAAABHNCSVQICAgIfAhkiAAAAAlwSFlz
             AAAKnQAACp0BJpU99gAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAABSdEVY
             dENvcHlyaWdodABDQyBBdHRyaWJ1dGlvbi1TaGFyZUFsaWtlIGh0dHA6Ly9jcmVhdGl2ZWNvbW1v
             bnMub3JnL2xpY2Vuc2VzL2J5LXNhLzQuMC/DVGIFAAABQklEQVQokY3RMWvCQBQH8H8ihsqdh0gH
             TYVL8kH8AG4dQrcunbp0rXM/QUahi0Oto/QTdNVFhVZId3EQISbnpaGFXKekaavYP7zhjvfjHu80
             fOfENM1bSulFvV4nABAEQSylfFoul3cAYuyJaVnWot/vJ0EQqO12m9dwOPxwHOcNgP0blW3bfpnN
             Zj9AsXzfV47j+AAqGSo1m82bbrfrttvt8r5RAIBSilarxSaTiS6EeAYAnRBy6bpu5RDK0ul0DELI
             eXbWGWNVwzCOOWiahlqtVs2hUuooypKmad6sCyHCJEn+gxCGochhFEX3g8FAHoOj0SiJ4/ixeFfi
             nE/H43F66Dvm87myLGsB4M8yTjnnU8/z5Hq9zsFms1G9Xu+dc/4K4KwItOLLjUbjmlJ6xRhjSilt
             t9sJKeXDarXyAHwW4Rfhy7qDi3SfrAAAAABJRU5ErkJggg==
          }

          # rb-sa
          set imgdata(rb-sa) {
             iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABHNCSVQICAgIfAhkiAAAAAlwSFlz
             AAAOJgAADiYBou8l/AAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAABSdEVY
             dENvcHlyaWdodABDQyBBdHRyaWJ1dGlvbi1TaGFyZUFsaWtlIGh0dHA6Ly9jcmVhdGl2ZWNvbW1v
             bnMub3JnL2xpY2Vuc2VzL2J5LXNhLzQuMC/DVGIFAAAC30lEQVQ4jYWRbWiVZRjHf/fL85zznJ0t
             B7WaZ20qErZ0W1s6ZA1JalAkyNqXZl9jJAgVFluFjJAIehmRfRj0QfoUhPRCMDLBo1CCq3biuJVL
             l7nyuHVqO3PuvDz3c999Upbr5f/p+vD//a+L6y+4RW+/+u42GVcHheNR61xCCuEcziil0+VSceTg
             K8+eXu0XN4bR0VGvshSNSqWfaO3YWn33hpSIxXwAjDFcmb1K5ttzheJKcbxoVd/g4EDhZsDw8LCu
             DepONm5qaO/s6khIKbHOkctfw0SW1O3VaK0AyGamKpPfn/+15FT74OBAQQPUVt35TkPj+vt3dm9P
             mMhy5KMzfHH2J0IT4ZzA04KOLSmGntrFtrZmXwiZmsz88DGwW4wMj2zSyWCi98k9NQh45o3PyM7M
             Ua6Yv/1GScnG9es4+nIfibjH58eOLxXyi71SxP2nm1u2JJSSfDCWITszvwYGiKzl59wih4+eBKBt
             +9YaPxF7QUql9jY01muAT05PUa6Ea+CbIZHlu+kc5dBQn7oLY6Kd0lpbl6yuohJGlMK1m29VxRh+
             yS2ilERpJaQA5xwIsarT/5BwAilvOB1SSJlbXlrG04og5v1vQMxXbKivxRhDFDkrIxMdu3zptxCg
             /+FWquL+v8JaSbpamtBKcmX2KkqKU7JSjt7/cXK6GIYhfbvvY0dzA4n42ks8Lbmn8Q5e3NeNc46J
             8WyhXCq9rk6kx5Ye63k8mZ9faN+4ucnv2bGZwPeY/b1AzNMkA5/bkgF7u+/ltYFH8LRiYvxcMT+X
             //K5lw68pQH+uD53yM2LtvTxr3Y9+FBnsr+nlf6eVpZXyhjrWJeMA2Ct5ZszmeLF6ZkLkVfeB6AA
             0um06+x64ENToub81IUWnFDxIC6TyYAg5nF9eYVLFy+7Uye+vvZnfuHThWKwZ2hofwn+obk3Dx9p
             Ulrs10r2RpGtcyCUVAsWN2ZL5r3nDx3Irvb/BdytK8PnPThIAAAAAElFTkSuQmCC
          }
          # rb-sd
          set imgdata(rb-sd) {
             iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABHNCSVQICAgIfAhkiAAAAAlwSFlz
             AAAOJgAADiYBou8l/AAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAABSdEVY
             dENvcHlyaWdodABDQyBBdHRyaWJ1dGlvbi1TaGFyZUFsaWtlIGh0dHA6Ly9jcmVhdGl2ZWNvbW1v
             bnMub3JnL2xpY2Vuc2VzL2J5LXNhLzQuMC/DVGIFAAACwklEQVQ4jYWTS0hUURjH/+c7Z8Y7M7e0
             F47OSEpFZfigUuwllVTQmyArIoKolSEJFdQiLtimB2WJm4LWMYtoYVBW9LAylCErpcLAHjYpldU4
             rzv33HtalKWJ9Vsd+P6//+LjOwx/caq+scDt5vuI2GbbcXIAgBN9dBx1NZE0Lxwx6t6MzLPhRygU
             4pHewdOcqz1zi+Z486bnCn2CDgCIDcXw/m1Evnj+MmHb7FJuweSD1dXV9u8CwzDEJK+/JZCXXbao
             skznnAMALGkDSsHlEgAAKW203e+IRfoGOr4m+lcbhiEFAEzx+c/lBHPKl64o9ymlcONeO+61dcK0
             LABAhtuF5RWlWFVZhmUrK/SHdx+X4Z3TAGA/O1HfMNvn9bRv2bF+ImMMZy6G8PrNB6TT1qjdZLhd
             mFUQxIG9W+E4ClcuN0cTiWQ5aZpWW1xaqBMRmm89wuvesTIAmGkLPb19uHa7DZwTSkoLdU3TaglK
             bQrmB0gphQcdXUhbY+WRJa3tz6GUQjA/QFBqEzmOneX1efB9KA4p5bjyMFJKfB+Kw+vzwHHsLBoe
             KKWg1H/9X7k/QSLGY2bKROYEH4Sgf6g/EYIja6IOM2WCMYoTCC2Rvn5FRCieOwOcxi/hRCgpnAnG
             GCJ9/YoRtZCVNBs7w11RpRS2baxCwD8VxNkYmYgQyJmK6g0roZRCZ7graiVT53nLneuRNVVrK6Ql
             8wNBv1iysAiD36KIJ1IQnCPD7YLu82B+0SzU7N4Cl0vgabg79Xngy826o/vPCgAwMbTzVXdPmDE2
             vWTBPG3PtnWQto2BT4MAgOxpkyF+nffTcHfqZXfPW4viu4ARn6nJaNJtH4U0j7a4dEFRZm6eH0L8
             lKS0EXnfjycdz6KmmW7lcXt7jVETG1UwzOnjDZUZmnZISmcZ5+AAYNuwhaDWZCJ98vCx2taR+R+G
             cynsNneqLwAAAABJRU5ErkJggg==
          }
          # rb-sn
          set imgdata(rb-sn) {
             iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABHNCSVQICAgIfAhkiAAAAAlwSFlz
             AAAOJgAADiYBou8l/AAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAABSdEVY
             dENvcHlyaWdodABDQyBBdHRyaWJ1dGlvbi1TaGFyZUFsaWtlIGh0dHA6Ly9jcmVhdGl2ZWNvbW1v
             bnMub3JnL2xpY2Vuc2VzL2J5LXNhLzQuMC/DVGIFAAACyUlEQVQ4jYWRS2icVRiGn/Od/zL5TYek
             k4umjWlGW6kmxY5oW6gKokiEAWnduVQJFJRSW0hRahAXXahBVCTgxoXgRoq46MKA4wVdKPVWi8G0
             JM04TTo2M5N2Opf/P+e4iqaG6rP6Fu/zLr5X8S/efPXtURXIUZwbs85FopRzziWidSGJ21NHXz78
             5fq8Wjump6f9RiWeFq0PDt0xuKmnf7PyfQ8AYwwr5SoL54u1Vqv9XQvvqYmJ8drfBZOTk15Xqvfz
             TH8mt+Pu4UgphXWOcrVBYiy3dkdoLQAsXCi2i/OXii283MTEeM0DSIeZtzb3de++655sZKzjw5lz
             fHW2RGIMOIXWipHhHsafGGUouzUQkS2LF/44BTyipianshKFP+x56L40Cl754Ftmiyu0Y3vDb0QU
             g72dnHzmQTpCj++/+Xm1UasfEBd4z23dNhCJKE59PcdssbJBBrDWUSxf471PfwJgePtgWof+MRGR
             JzO93R7AzJmLtGOzQV7DWMevCyu0E0N3pgtr7T6x1valOkLixNJKbi6vESeG0p91RBSiRYmIOOcA
             3D+b/gcKUOuCglKXmteb+J4mFXj/WxD4wpaeTowxWOusmMR8XF6+EgPk92SJwpuXeKLIbe/H08JK
             uYqI+kKS2L1furjUMMbw+P1DjA73kgr0BtnXwrbbunh2bATnHPO/L9biVuuknimcXh17LN+5Wr2W
             6x/oCfaPDJAKPJYqdQJP05Hy2BQFPLr7do4czOF7mvm5xUatsvrZiy+98IYHUGksn7DO3nv2zOzD
             O3fd2ZnfmyW/N8v1ZkxiHekoAMA5x/nf5htLpctzLmg/DaABCoWC27f/gY+IdbpUXN6FQwehL1FH
             SOhrmo0Wl5euuHM/zl6tX61/Um3ekj9+/FBzbZUbeP21d4aUuENa5IC1tg+llCipONxp17bvHjnx
             /C/r838BIX8q3IsYTesAAAAASUVORK5CYII=
          }
          # rb-ua
          set imgdata(rb-ua) {
             iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABHNCSVQICAgIfAhkiAAAAAlwSFlz
             AAAOJgAADiYBou8l/AAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAABSdEVY
             dENvcHlyaWdodABDQyBBdHRyaWJ1dGlvbi1TaGFyZUFsaWtlIGh0dHA6Ly9jcmVhdGl2ZWNvbW1v
             bnMub3JnL2xpY2Vuc2VzL2J5LXNhLzQuMC/DVGIFAAACAUlEQVQ4jZWT30tTYRjHv+/zHs9+qANv
             VrJl4lXJ1KWJiPQHdCHE6ia6DkEoKpQmxBjhRRfRCOti0B/QjUQQBBK2RSWxYoupZRqImlmpa2vu
             nG3ved9uUoaQeD5Xz8X38+V5Lh6Gfdy7PdFBTj7CFM5KpdzEmFJQgnMtUTaN2Mita69q82x3iMfj
             dZWCFSeune/qCTQea/Uxh0MHAAghsL66gcyH2bxRMlKG5BfC4aH8XkE0GtWaXN6XLW3+7r6BHjcR
             7V9sj2xmvjL3cWHNVLw7HB7KEwA01R+572/1neo/03ugDAAdwXY9EDzpczHrCQCwWDTWpjW40qGL
             gx7OD5ZreTY5Vchv/g4Rc+qX2ztPuO3IABDsDXh0t2OUiPNz/pZmzZYNoNl3FEJY/SSl9DY01tv1
             wTmBa5wRA5RStv1/KBAj+l4sFG2rQghYlpJkCWtyZflb1W7B+uoGOLEkVcrWo89zX4xq9fAdSimk
             U9l82TTv0M3o1TUGTLyefnfoO9KpWaNSrkyPRq7PEABs7fyI/Pq5lUxMvSmKqvivKKXE+5mMsfhp
             canKS5cAgANAIpFQfQOnHwsTnoX5pU4oxp0uJ+mOOjDGsFMsYfnrikq+ePtnezP3NGe4BsfGhk2g
             5ht3uTv+4DjX2LDGKWRZ0qsAxonnJNRzaYqHNyJXsrX5vxZq0kmvqutEAAAAAElFTkSuQmCC
          }
          # rb-ud
          set imgdata(rb-ud) {
             iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABHNCSVQICAgIfAhkiAAAAAlwSFlz
             AAAOJgAADiYBou8l/AAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAABSdEVY
             dENvcHlyaWdodABDQyBBdHRyaWJ1dGlvbi1TaGFyZUFsaWtlIGh0dHA6Ly9jcmVhdGl2ZWNvbW1v
             bnMub3JnL2xpY2Vuc2VzL2J5LXNhLzQuMC/DVGIFAAABqElEQVQ4jZWTPYsTURSG3/c6ytzATMIG
             tjCFYG9hGJJYprRbVjtbLbTW1lLsRMRC8C+IhYWdcW0s8mFha7kQYV2T+VhnxuTOayEucVHXeerz
             PIdTHOIEk8nkkqS7JK8CaAAQgDXJt865R71e793mPDfEs6SeAeZaq9UMrLU0xgAAJKEoCiwWy9g5
             NyZ5PYqi+DgwGo28IAhG1vrdra2tBkn8jTiOv6dptg+gG0VRbAAgCILH1vqX2+32P2UAaDab58Iw
             7Eh6CQAcj8cXSX7odM6Hp8mbzOefk9VqtWtI3grD8NTNJ2m1mqEx5p4BsGOt79WyAfi+D0lXjKRt
             z6vtgyRI0pBUbfsYwQCYr9fr+qoEgJWR9OLo6NuqbqAoCkjaM86551mW5VVV1QosFsu4qqqHZjAY
             7JN6cnj4NftfOY6TXNKbfr//3gBAtxvdL8ty7+DgS/bztj8jCctlnCdJ8qksyxvAxjNJMtPp9AHJ
             20EQNKy1nuedAUk455DnueI4SSW9StP05nA4LH4L/GI2m11wzt0huStpGwCNMYuqql6TfBpF0cfN
             +R+IaMQGt5KnggAAAABJRU5ErkJggg==
          }
          # rb-un
          set imgdata(rb-un) {
             iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABHNCSVQICAgIfAhkiAAAAAlwSFlz
             AAAOJgAADiYBou8l/AAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAABSdEVY
             dENvcHlyaWdodABDQyBBdHRyaWJ1dGlvbi1TaGFyZUFsaWtlIGh0dHA6Ly9jcmVhdGl2ZWNvbW1v
             bnMub3JnL2xpY2Vuc2VzL2J5LXNhLzQuMC/DVGIFAAAB8ElEQVQ4jZWTTWsTYRSFz3tn8mHE0DQ2
             BdsaG1FBUbCCUhD3LgSp7lxLoaCotDQFCYO4cCEGqS4C/gA3Iq7caepCF4rfqMVUUjOkiaFNpm2c
             TGbe97qKBqGl86zu4jyHuzkC/3HnxuxhEaRJMJ9WzBESgpnZI03Le247O3n9yovuvOgcuVwuYNfd
             HGnaueTeoR07+3tFIKADAKSUWKk1sLhgWo7Tfu1AP59Oj1t/CwzD0HvCfc/j/fGR/QeHI0IIbMTi
             D7NtFpdMB/pIOj1uEQBEQ/G7vYnY0QOHUpvKAJBMDQZ3pwYGwvAeA4DIGtkURULvTpw6FiXaXO7m
             zcuPq7bVHCMO6hcH9+yK+JEBYHjfUFQLBaaIiM7G+2K6LxtALN4DpdQoKaUS4W0hvz6IBEgjQUTE
             zL79f0UQYqn1u+VblFJCKVYkPfmoVl12/Ras1BogEnPkufyg/LNiSym3LDMzit9Llus4t2jauGwK
             QbNfPxTWt1pQLJRs13WfTWWuviIAqNvVjNVYnfv8dn5deht/wsxY+Fa0y6VKQQWdCwCgAUA+n+fR
             k8cfwtWiZbN6BAwtGApQZ0wt28GvyjJ/eT+/1lxrPmm0tp+ZmZloAV1r7HD75r2kIJ7QiMaUUgkI
             IUhQncFPua3uX8tc+tSd/wNrt9JgtBf0OwAAAABJRU5ErkJggg==
          }
        }
      }

      proc _createTheme { } {
        variable imgdata
        variable images
        variable vars

        # only the styling is set here.
        # the colors are set in setStyledColors

        ttk::style theme create $vars(theme.name) -parent clam -settings {

          # menu

          foreach {n} {menu-cb-un-pad menu-cb-sn-pad menu-rb-un-pad menu-rb-sn-pad} {
            set images($n) [image create photo -data $imgdata($n)]
          }

          # small checkbutton and radiobutton images

          foreach {n} {cb-un-small cb-sn-small rb-un-small rb-sn-small} {
            set images($n) [image create photo -data $imgdata($n)]
          }

          # button

          ttk::style configure TButton \
              -anchor {} \
              -width -8 \
              -padding {5 4} \
              -borderwidth 1 \
              -relief raised

          # checkbutton

          foreach {n} {cb-un cb-ud cb-sn cb-sd cb-sa cb-ua} {
            set images($n) [image create photo -data $imgdata($n)]
          }

          ttk::style element create Checkbutton.indicator image \
              [list $images(cb-un) \
              {hover selected !disabled} $images(cb-sa) \
              {hover !selected !disabled} $images(cb-ua) \
              {!selected disabled} $images(cb-ud) \
              {selected disabled} $images(cb-sd) \
              {selected !disabled} $images(cb-sn)]

          ttk::style layout TCheckbutton {
            Checkbutton.focus -side left -sticky w -children {
              Checkbutton.indicator -side left -sticky {}
              Checkbutton.padding -sticky nswe -children {
                Checkbutton.label -sticky nswe
              }
            }
          }

          ttk::style configure TCheckbutton \
              -padding {5 1 1 1} \
              -borderwidth 1 \
              -relief none \
              -focusthickness 4

          ttk::style element create Menu.Checkbutton.indicator image \
              [list $images(cb-un-small) \
              {selected !disabled} $images(cb-sn-small)]

          ttk::style layout Menu.TCheckbutton {
            Checkbutton.padding -sticky nswe -children {
              Menu.Checkbutton.indicator -side left -sticky {}
            }
          }

          ttk::style configure Menu.TCheckbutton \
              -padding {5 1 1 1} \
              -borderwidth 0 \
              -relief none \
              -focusthickness 0

          # combobox

          ttk::style configure TCombobox \
              -padding {5 2} \
              -borderwidth 1 \
              -arrowsize 15 \
              -relief none

          bind ComboboxListbox <Map> \
              [list +::ttk::theme::${vars(theme.name)}::awCboxHandler %W]

          # entry

          ttk::style configure TEntry \
              -padding {5 2} \
              -borderwidth 1 \
              -relief none

          # labelframe

          ttk::style configure TLabelframe \
              -borderwidth 1 \
              -relief groove

          # menubutton

          ttk::style configure TMenubutton \
              -padding {5 2} \
              -relief none

          # notebook

          _createNotebookStyle

          ttk::style configure TNotebook \
              -borderwidth 0
          ttk::style configure TNotebook.Tab \
              -padding {1 0 1 0} \
              -focusthickness 5 \
              -borderwidth 0

          # panedwindow

          ttk::style configure Sash \
              -sashthickness 8

          # progressbar

          ttk::style configure TProgressbar \
              -borderwidth 1 \
              -pbarrelief none

          # radiobutton

          foreach {n} {rb-un rb-ud rb-sn rb-sd rb-ua rb-sa} {
            set images($n) [image create photo -data $imgdata($n)]
          }

          ttk::style element create Radiobutton.indicator image \
              [list $images(rb-un) \
              {hover selected !disabled} $images(rb-sa) \
              {hover !selected !disabled} $images(rb-ua) \
              {!selected disabled} $images(rb-ud) \
              {selected disabled} $images(rb-sd) \
              {selected !disabled} $images(rb-sn)]

          ttk::style layout TRadiobutton {
            Radiobutton.focus -side left -sticky w -children {
              Radiobutton.indicator -side left -sticky {}
              Radiobutton.padding -sticky nswe -children {
                Radiobutton.label -sticky nswe
              }
            }
          }

          ttk::style configure TRadiobutton \
              -padding {5 1 1 1} \
              -borderwidth 1 \
              -relief none \
              -focusthickness 4

          ttk::style element create Menu.Radiobutton.indicator image \
              [list $images(rb-un-small) \
              {selected} $images(rb-sn-small)]

          ttk::style layout Menu.TRadiobutton {
            Radiobutton.padding -sticky nswe -children {
              Menu.Radiobutton.indicator -side left -sticky {}
            }
          }

          ttk::style configure Menu.TRadiobutton \
              -padding {5 1 1 1} \
              -borderwidth 0 \
              -relief none \
              -focusthickness 0

          # scale

          ttk::style configure TScale \
              -borderwidth 1

          # scrollbar

          ttk::style configure TScrollbar \
              -borderwidth 0

          # spinbox

          ttk::style configure TSpinbox \
              -padding {5 2} \
              -borderwidth 1 \
              -relief none \
              -arrowsize 15
        }
      }

      proc _createNotebookStyle { } {
        variable colors
        variable images
        variable vars

        set tag "$colors(curr.tabhighlight)$colors(curr.tabinactive)$colors(curr.selectbg)"
        foreach {k bg} [list \
            indhover $colors(curr.tabhighlight) \
            indnotactive $colors(curr.tabinactive) \
            indselected $colors(curr.selectbg) \
            ] {
          if { ! [info exists images($k.$bg)] } {
            set images($k.$bg) [image create photo \
                -width $vars(nb.img.width) -height $vars(nb.img.height)]
            set row [lrepeat $vars(nb.img.width) $bg]
            set pix [list]
            for {set i 0} {$i < $vars(nb.img.height)} {incr i} {
              lappend pix $row
            }
            $images($k.$bg) put $pix
          }
          set images($k) $images($k.$bg)
        }

        if { ! [info exists vars(cache.nb.ind.${tag})] } {
          ttk::style element create \
             Notebook.indicator.${tag} image \
             [list $images(indnotactive) \
             {hover active !selected !disabled} $images(indhover) \
             {selected !disabled} $images(indselected)]
          set vars(cache.nb.ind.${tag}) true
        }

        ttk::style layout TNotebook.Tab [list \
          Notebook.tab -sticky nswe -children [list \
            Notebook.padding -side top -sticky nswe -children [list \
              Notebook.indicator.${tag} -side top -sticky we \
              Notebook.focus -side top -sticky nswe -children {
                Notebook.label -side top -sticky {} \
              } \
            ] \
          ] \
        ]
      }

      proc _setStyledColors { {theme {}} } {
        variable colors
        variable vars

        if { $theme eq {} } {
          set theme $vars(theme.name)
        }

        ttk::style theme settings $theme {
          # defaults
          ttk::style configure . \
              -background $colors(curr.frame) \
              -foreground $colors(curr.text) \
              -borderwidth 1 \
              -bordercolor $colors(curr.border) \
              -darkcolor $colors(curr.darkest) \
              -lightcolor $colors(curr.darkest) \
              -troughcolor $colors(curr.entrybg) \
              -selectbackground $colors(curr.selectbg) \
              -selectforeground $colors(curr.selectfg) \
              -selectborderwidth 0 \
              -fieldbackground $colors(curr.entrybg) \
              -focuscolor $colors(curr.selectbg) \
              -insertcolor $colors(curr.lightest) \
              -relief none
          ttk::style map . \
              -background [list disabled $colors(curr.frame)] \
              -foreground [list active $colors(curr.text) \
                  focus $colors(curr.text) \
                  disabled $colors(curr.disabledfg)] \
              -selectbackground [list !focus $colors(curr.darkest)] \
              -selectforeground [list !focus $colors(curr.lightest)] \
              -bordercolor [list disabled $colors(curr.disabledborder)]

          # button

          ttk::style configure TButton \
              -bordercolor $colors(curr.frame) \
              -background $colors(curr.dark) \
              -lightcolor $colors(curr.lighter) \
              -darkcolor $colors(curr.darker)
          ttk::style map TButton \
              -background [list {hover !pressed !disabled} $colors(curr.bpress) \
                  {active !pressed} $colors(curr.bpress) \
                  {selected !disabled} $colors(curr.dark) \
                  pressed $colors(curr.dark) \
                  disabled $colors(curr.disabledbg)] \
              -lightcolor [list pressed $colors(curr.darker)] \
              -darkcolor [list pressed $colors(curr.lighter)]

          # checkbutton

          ttk::style map TCheckbutton \
              -indicatorcolor [list selected $colors(curr.lightest)] \
              -darkcolor [list disabled $colors(curr.frame)] \
              -lightcolor [list disabled $colors(curr.frame)]

          # combobox

          ttk::style configure TCombobox \
              -bordercolor $colors(curr.border) \
              -lightcolor $colors(curr.dark) \
              -darkcolor $colors(curr.dark) \
              -arrowcolor $colors(curr.arrow)
          ttk::style map TCombobox \
              -lightcolor [list active $colors(curr.selectbg) \
                  focus $colors(curr.selectbg)] \
              -darkcolor [list active $colors(curr.selectbg) \
                  focus $colors(curr.selectbg)] \
              -arrowcolor [list disabled $colors(curr.arrow.disabled)] \
              -fieldbackground [list disabled $colors(curr.disabledbg)]
          if { $::tcl_platform(os) eq "Darwin" } {
            # mac os x has cross-platform incompatibilities
            ttk::style configure TCombobox \
                -background $colors(curr.dark)
            ttk::style map TCombobox \
                -background [list disabled $colors(curr.disabledbg)]
          }

          # entry

          ttk::style configure TEntry \
              -background $colors(curr.dark) \
              -bordercolor $colors(curr.border) \
              -lightcolor $colors(curr.dark)
          ttk::style map TEntry \
              -lightcolor [list active $colors(curr.selectbg) \
                  focus $colors(curr.selectbg)] \
              -fieldbackground [list disabled $colors(curr.disabledbg)]
          if { $::tcl_platform(os) eq "Darwin" } {
            # mac os x has cross-platform incompatibilities
            ttk::style configure TEntry \
                -background $colors(curr.dark)
            ttk::style map TEntry \
                -background [list disabled $colors(curr.disabledbg)]
          }

          # labelframe

          ttk::style configure TLabelframe \
              -lightcolor $colors(curr.frame) \
              -darkcolor $colors(curr.frame)

          # menubutton

          ttk::style configure TMenubutton \
              -arrowcolor $colors(curr.arrow)
          ttk::style map TMenubutton \
              -background [list {active !disabled} $colors(curr.selectbg)] \
              -foreground [list {active !disabled} $colors(curr.selectfg) \
                  disabled $colors(curr.disabledfg)] \
              -arrowcolor [list disabled $colors(curr.arrow.disabled)]

          # notebook

          ttk::style configure TNotebook \
              -bordercolor $colors(curr.frame) \
              -lightcolor $colors(curr.lighter) \
              -darkcolor $colors(curr.darker)
          ttk::style configure TNotebook.Tab \
              -lightcolor $colors(curr.frame) \
              -darkcolor $colors(curr.frame) \
              -bordercolor $colors(curr.tabborder) \
              -background $colors(curr.dark)

          # panedwindow

          ttk::style configure TPanedwindow \
              -background $colors(curr.dark)
          ttk::style configure Sash \
              -lightcolor $colors(curr.darkhighlight) \
              -darkcolor $colors(curr.darkest)

          # progressbar

          ttk::style configure TProgressbar \
              -background $colors(curr.darkhighlight) \
              -bordercolor $colors(curr.border.light) \
              -lightcolor $colors(curr.darkhighlight) \
              -darkcolor $colors(curr.darkhighlight)
          ttk::style map TProgressbar \
              -troughcolor [list disabled $colors(curr.dark)] \
              -darkcolor [list disabled $colors(curr.disabledbg)] \
              -lightcolor [list disabled $colors(curr.disabledbg)]

          # scale

          # background is used both for the background and
          # for the grip colors
          ttk::style configure TScale \
              -background $colors(curr.darkhighlight) \
              -bordercolor $colors(curr.border.light) \
              -lightcolor $colors(curr.darkhighlight) \
              -darkcolor $colors(curr.darkhighlight)
          ttk::style map TScale \
              -troughcolor [list disabled $colors(curr.dark)] \
              -darkcolor [list disabled $colors(curr.disabledbg)] \
              -lightcolor [list disabled $colors(curr.disabledbg)]

          # scrollbar

          ttk::style configure TScrollbar \
              -background $colors(curr.selectbg) \
              -bordercolor $colors(curr.border.light) \
              -lightcolor $colors(curr.selectbg) \
              -darkcolor $colors(curr.selectbg) \
              -arrowcolor $colors(curr.lightest)
          ttk::style map TScrollbar \
              -arrowcolor [list disabled $colors(curr.arrow.disabled)] \
              -darkcolor [list disabled $colors(curr.frame)] \
              -lightcolor [list disabled $colors(curr.frame)]

          # spinbox

          ttk::style configure TSpinbox \
              -bordercolor $colors(curr.border) \
              -lightcolor $colors(curr.dark) \
              -arrowcolor $colors(curr.arrow)
          ttk::style map TSpinbox \
              -lightcolor [list active $colors(curr.selectbg) \
                  focus $colors(curr.selectbg)] \
              -darkcolor [list active $colors(curr.selectbg) \
                  focus $colors(curr.selectbg)] \
              -arrowcolor [list disabled $colors(curr.arrow.disabled)] \
              -fieldbackground [list disabled $colors(curr.disabledbg)]
          if { $::tcl_platform(os) eq "Darwin" } {
            # mac os x has cross-platform incompatibilities
            ttk::style configure TSpinbox \
                -background $colors(curr.dark)
            ttk::style map TSpinbox \
                -background [list disabled $colors(curr.disabledbg)]
          }

          # treeview

          ttk::style configure Treeview \
              -fieldbackground $colors(curr.frame)
          ttk::style map Treeview \
              -background [list selected $colors(curr.selectbg)] \
              -foreground [list selected $colors(curr.selectfg)]
        }
      }

      proc setBackground { bcol } {
        variable colors
        variable vars

        if { ! [package present colorutils] } {
          return
        }

        foreach {k} [array names colors base.*] {
          regsub {^base} $k curr nk
          set tc [::colorutils::adjustColor $colors($k) $colors(base.frame) $bcol]
          set colors($nk) $tc
        }

        _setColors
      }

      proc setHighlight { hcol } {
        variable colors
        variable vars

        if { ! [package present colorutils] } {
          return
        }

        foreach {k} [array names colors highlight.*] {
          regsub {^highlight} $k curr nk
          set tc [::colorutils::adjustColor $colors($k) $colors(highlight.selectbg) $hcol]
          set colors($nk) $tc
        }

        _setColors
      }

      proc _setColors { } {
        variable vars

        _setStyledColors
        dict for {w v} $vars(cache.menu) {
          if { [winfo exists $w] } {
            setMenuColors $w
          }
        }
        dict for {w t} $vars(cache.text) {
          if { [winfo exists $w] } {
            setTextColors $w $t
          }
        }
        dict for {w v} $vars(cache.listbox) {
          if { [winfo exists $w] } {
            setListboxColors $w
          }
        }

        _createNotebookStyle
      }

      proc setMenuColors { w } {
        variable colors
        variable images
        variable vars

        if { ! [dict exists $vars(cache.menu) $w] } {
          dict set vars(cache.menu) $w 1
        }

        # need to figure out under what conditions this fails
        # on mac os x.
        if { $::tcl_platform(os) ne "Darwin" } {
          $w configure -background $colors(curr.frame)
          $w configure -foreground $colors(curr.text)
          $w configure -activebackground $colors(curr.selectbg)
          $w configure -activeforeground $colors(curr.selectfg)
          $w configure -disabledforeground $colors(curr.disabledfg)
          $w configure -selectcolor $colors(curr.selectbg)

          set max [$w index end]
          if { $max eq "none" } {
            return
          }

          # the standard menu does not have a -mode option
          if { [catch {$w cget -mode}] } {
            # this does not work for mac os x standard menus.
            # it is fine for user menus or pop-ups
            for {set i 0} {$i <= $max} {incr i} {
              set type [$w type $i]
              if { $type eq "checkbutton" } {
                $w entryconfigure $i \
                    -image $images(menu-cb-un-pad) \
                    -selectimage $images(menu-cb-sn-pad) \
                    -compound left \
                    -hidemargin 1
              }
              if { $type eq "radiobutton" } {
                $w entryconfigure $i \
                    -image $images(menu-rb-un-pad) \
                    -selectimage $images(menu-rb-sn-pad) \
                    -compound left \
                    -hidemargin 1
              }
            } ; # for each menu entry
          } ; # using flexmenu? (has -mode)
        } ; # not darwin
      }

      proc setListboxColors { w } {
        variable colors
        variable images
        variable vars

        if { ! [dict exists $vars(cache.listbox) $w] } {
          dict set vars(cache.listbox) $w 1
        }

        $w configure -background $colors(curr.frame)
        $w configure -foreground $colors(curr.text)
        $w configure -disabledforeground $colors(curr.disabledfg)
        $w configure -selectbackground $colors(curr.selectbg)
        $w configure -selectforeground $colors(curr.selectfg)
        $w configure -borderwidth 1p
        $w configure -relief solid
      }

      proc setTextColors { w {darkflag {}} } {
        variable colors
        variable images
        variable vars

        if { ! [dict exists $vars(cache.text) $w] } {
          dict set vars(cache.text) $w $darkflag
        }

        if { $darkflag ne {} } {
          $w configure -background $colors(curr.dark)
        } else {
          $w configure -background $colors(curr.frame)
        }
        $w configure -foreground $colors(curr.text)
        $w configure -selectforeground $colors(curr.selectfg)
        $w configure -selectbackground $colors(curr.selectbg)
        $w configure -inactiveselectbackground $colors(curr.darkest)
        $w configure -borderwidth 0
      }

      proc awCboxHandler { w } {
        variable vars
        variable colors

        set theme [ttk::style theme use]
        if { [info exists colors(curr.entrybg)] &&
            $theme eq $vars(theme.name) &&
            ! [dict exists $vars(cache.listbox) $w] } {
          ::ttk::theme::${vars(theme.name)}::setListboxColors $w
        }
      }

      init
    }
    package provide ttk::theme::$awthemename 2.3
  }
}

awinit
unset -nocomplain ::awthemename
