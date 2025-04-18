------------------------------------------------------------------------------
-- A few helper operations which should be part of the Prelude.
------------------------------------------------------------------------------

module System.PreludeHelpers
 where

------------------------------------------------------------------------------
-- Read/Show instances for larger tuples.

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g) =>
         Show (a, b, c, d, e, f, g) where
  showsPrec _ (a, b, c, d, e, f, g) =
    showTuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g]

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h) =>
         Show (a, b, c, d, e, f, g, h) where
  showsPrec _ (a, b, c, d, e, f, g, h) =
    showTuple [ shows a, shows b, shows c, shows d, shows e, shows f
              , shows g, shows h]

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h,
          Show i) =>
         Show (a, b, c, d, e, f, g, h, i) where
  showsPrec _ (a, b, c, d, e, f, g, h, i) =
    showTuple [ shows a, shows b, shows c, shows d, shows e, shows f
              , shows g, shows h, shows i]

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h,
          Show i, Show j) =>
         Show (a, b, c, d, e, f, g, h, i, j) where
  showsPrec _ (a, b, c, d, e, f, g, h, i, j) =
    showTuple [ shows a, shows b, shows c, shows d, shows e, shows f
              , shows g, shows h, shows i, shows j]

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h,
          Show i, Show j, Show k) =>
         Show (a, b, c, d, e, f, g, h, i, j, k) where
  showsPrec _ (a, b, c, d, e, f, g, h, i, j, k) =
    showTuple [ shows a, shows b, shows c, shows d, shows e, shows f
              , shows g, shows h, shows i, shows j, shows k]

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h,
          Show i, Show j, Show k, Show l) =>
         Show (a, b, c, d, e, f, g, h, i, j, k, l) where
  showsPrec _ (a, b, c, d, e, f, g, h, i, j, k, l) =
    showTuple [ shows a, shows b, shows c, shows d, shows e, shows f
              , shows g, shows h, shows i, shows j, shows k, shows l]


instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g) =>
    Read (a, b, c, d, e, f, g) where
  readsPrec _ = readParen False
                  (\o -> [ ((a, b, c, d, e, f, g), z) | ("(", p) <- lex o
                                                   , (a, q) <- reads p
                                                   , (",", r) <- lex q
                                                   , (b, s) <- reads r
                                                   , (",", t) <- lex s
                                                   , (c, u) <- reads t
                                                   , (",", v) <- lex u
                                                   , (d, w) <- reads v
                                                   , (",", x) <- lex w
                                                   , (e, y) <- reads x
                                                   , (",", z1) <- lex y
                                                   , (f, z2) <- reads z1
                                                   , (",", z3) <- lex z2
                                                   , (g, z4) <- reads z3
                                                   , (")", z) <- lex z4 ])

instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g, Read h) =>
    Read (a, b, c, d, e, f, g, h) where
  readsPrec _ = readParen False
                  (\o -> [ ((a, b, c, d, e, f, g, h), z) | ("(", p) <- lex o
                                                   , (a, q) <- reads p
                                                   , (",", r) <- lex q
                                                   , (b, s) <- reads r
                                                   , (",", t) <- lex s
                                                   , (c, u) <- reads t
                                                   , (",", v) <- lex u
                                                   , (d, w) <- reads v
                                                   , (",", x) <- lex w
                                                   , (e, y) <- reads x
                                                   , (",", z1) <- lex y
                                                   , (f, z2) <- reads z1
                                                   , (",", z3) <- lex z2
                                                   , (g, z4) <- reads z3
                                                   , (",", z5) <- lex z4
                                                   , (h, z6) <- reads z5
                                                   , (")", z) <- lex z6 ])

instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g, Read h,
          Read i) =>
    Read (a, b, c, d, e, f, g, h, i) where
  readsPrec _ = readParen False
                  (\o -> [ ((a, b, c, d, e, f, g, h, i), z) | ("(", p) <- lex o
                                                   , (a, q) <- reads p
                                                   , (",", r) <- lex q
                                                   , (b, s) <- reads r
                                                   , (",", t) <- lex s
                                                   , (c, u) <- reads t
                                                   , (",", v) <- lex u
                                                   , (d, w) <- reads v
                                                   , (",", x) <- lex w
                                                   , (e, y) <- reads x
                                                   , (",", z1) <- lex y
                                                   , (f, z2) <- reads z1
                                                   , (",", z3) <- lex z2
                                                   , (g, z4) <- reads z3
                                                   , (",", z5) <- lex z4
                                                   , (h, z6) <- reads z5
                                                   , (",", z7) <- lex z6
                                                   , (i, z8) <- reads z7
                                                   , (")", z) <- lex z8 ])

instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g, Read h,
          Read i, Read j) =>
    Read (a, b, c, d, e, f, g, h, i, j) where
  readsPrec _ = readParen False
                (\o -> [ ((a, b, c, d, e, f, g, h, i, j), z) | ("(", p) <- lex o
                                                   , (a, q) <- reads p
                                                   , (",", r) <- lex q
                                                   , (b, s) <- reads r
                                                   , (",", t) <- lex s
                                                   , (c, u) <- reads t
                                                   , (",", v) <- lex u
                                                   , (d, w) <- reads v
                                                   , (",", x) <- lex w
                                                   , (e, y) <- reads x
                                                   , (",", z1) <- lex y
                                                   , (f, z2) <- reads z1
                                                   , (",", z3) <- lex z2
                                                   , (g, z4) <- reads z3
                                                   , (",", z5) <- lex z4
                                                   , (h, z6) <- reads z5
                                                   , (",", z7) <- lex z6
                                                   , (i, z8) <- reads z7
                                                   , (",", z9) <- lex z8
                                                   , (j, z10) <- reads z9
                                                   , (")", z) <- lex z10 ])

instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g, Read h,
          Read i, Read j, Read k) =>
    Read (a, b, c, d, e, f, g, h, i, j, k) where
  readsPrec _ = readParen False
   (\o -> [ ((a, b, c, d, e, f, g, h, i, j, k), z) | ("(", p) <- lex o
                                                   , (a, q) <- reads p
                                                   , (",", r) <- lex q
                                                   , (b, s) <- reads r
                                                   , (",", t) <- lex s
                                                   , (c, u) <- reads t
                                                   , (",", v) <- lex u
                                                   , (d, w) <- reads v
                                                   , (",", x) <- lex w
                                                   , (e, y) <- reads x
                                                   , (",", z1) <- lex y
                                                   , (f, z2) <- reads z1
                                                   , (",", z3) <- lex z2
                                                   , (g, z4) <- reads z3
                                                   , (",", z5) <- lex z4
                                                   , (h, z6) <- reads z5
                                                   , (",", z7) <- lex z6
                                                   , (i, z8) <- reads z7
                                                   , (",", z9) <- lex z8
                                                   , (j, z10) <- reads z9
                                                   , (",", z11) <- lex z10
                                                   , (k, z12) <- reads z11
                                                   , (")", z) <- lex z12 ])

instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g, Read h,
          Read i, Read j, Read k, Read l) =>
    Read (a, b, c, d, e, f, g, h, i, j, k, l) where
  readsPrec _ = readParen False
   (\o -> [ ((a, b, c, d, e, f, g, h, i, j, k, l), z) | ("(", p) <- lex o
                                                   , (a, q) <- reads p
                                                   , (",", r) <- lex q
                                                   , (b, s) <- reads r
                                                   , (",", t) <- lex s
                                                   , (c, u) <- reads t
                                                   , (",", v) <- lex u
                                                   , (d, w) <- reads v
                                                   , (",", x) <- lex w
                                                   , (e, y) <- reads x
                                                   , (",", z1) <- lex y
                                                   , (f, z2) <- reads z1
                                                   , (",", z3) <- lex z2
                                                   , (g, z4) <- reads z3
                                                   , (",", z5) <- lex z4
                                                   , (h, z6) <- reads z5
                                                   , (",", z7) <- lex z6
                                                   , (i, z8) <- reads z7
                                                   , (",", z9) <- lex z8
                                                   , (j, z10) <- reads z9
                                                   , (",", z11) <- lex z10
                                                   , (k, z12) <- reads z11
                                                   , (",", z13) <- lex z12
                                                   , (l, z14) <- reads z13
                                                   , (")", z) <- lex z14 ])

------------------------------------------------------------------------------
