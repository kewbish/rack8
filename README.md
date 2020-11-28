# rack8
A basic [Chip-8](https://en.wikipedia.org/wiki/CHIP-8) emulator, written in Racket.
Made by [Kewbish](https://github.com/kewbish).  
Created in Racket, October 2020 - present.  
Released under [GNU GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html).   
I've written about the process of making this on my blog, starting with [this post](https://kewbi.sh/blog/posts/201115/).

## Misc. Information
- not an entirely faithful reproduction - does not operate exactly on 60Hz.
- no sound implementation (it's in the command line, after all)
- press <kbd>p</kbd> to quit, will dump all executed instructions on the screen.
- Keymaps:
```text
+---+---+---+---+            +---+---+---+---+
| 0 | 1 | 2 | 3 |            | 1 | 2 | 3 | 4 |
+---------------+            +---------------+
| 4 | 5 | 6 | 7 |            | Q | W | E | R |
+---------------+   +---->   +---------------+
| 8 | 9 | A | B |            | A | S | D | F |
+---------------+            +---------------+
| C | D | E | F |            | Z | X | C | V |
+---+---+---+---+            +---+---+---+---+
```
