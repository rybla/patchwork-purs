# patchwork-purs

```sh
bun install
bun run Simple1
```

## TODO

- [x] validate chosen turn action 
  - [x] can only choose "buy" if you can afford at least one of the patches
    available to buy
  - [x] can only choose "wait" if you have non-0 time left
- [x] validate button/time spend
  - [x] this must happen inside of widget, before raising output
- [x] validate patch placement
  - [x] this must happen inside of widget, before raising output
- [ ] rules for collecting leathers when spend certain amounts of time first
- [ ] detect end of game
- [x] keep running count of scores
- [ ] rule for collecting the bonus when you make a 7x7 square
- [ ] get buttons when you wait
- [ ] UI for choosing how long to wait
- [ ] get button income when passing certain times
  - [x] count buttons on quilt
- [x] in addition to time, each player must also keep track of what turn of the
  game they last moved on, so that when both players have equal time, the player
  that moved on the more recent turn actually gets to move next