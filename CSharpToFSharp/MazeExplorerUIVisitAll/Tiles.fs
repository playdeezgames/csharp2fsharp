module Tiles

let room = RoomTiles.createCardinal Colors.Transparent Colors.Emerald

let explorer = ExplorerTiles.createCardinal Colors.Transparent Colors.Silver

let font = FontTiles.create Colors.Transparent Colors.Garnet

let Filled = new Tile.Tile(CommonPatterns.Filled, Colors.Transparent, Colors.Amethyst)
let Empty = new Tile.Tile(CommonPatterns.Empty, Colors.Onyx, Colors.Onyx)
