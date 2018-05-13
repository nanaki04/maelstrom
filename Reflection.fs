namespace Maelstrom

module Reflection =
  let log msg maelstrom =
    { maelstrom with reflection = msg :: maelstrom.reflection }
  let clear maelstrom =
    { maelstrom with reflection = List.empty }