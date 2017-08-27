-- SYNTAX TEST "Packages/Haskell/Haskell.sublime-syntax"

module SyntaxTest (
  traverse,   -- :: Applicative f => (a -> f b) -> t a -> f (t b)
#if 0
  sequenceA,  -- :: Applicative f ⇒ t (f a) → f (t a)
  (),
  (-->),
  (Modl.==>$),     -- :: Parse a -> Parse b -> Parse b
#endif
  MyList,
) where


import A
import A()
import A(x)
import qualified A
import qualified A()
import qualified A(x)
import A hiding ()
import A hiding (x)
import qualified A hiding ()
import qualified A hiding (x)
import A as B
import A as B(x)
import qualified A as B


module Stack( StkType, push, pop, empty ) where
  data StkType a = EmptyStk | Stk a (StkType a)
  push x s = Stk x s
  pop (Stk _ s) = s
  empty = EmptyStk

module Stack( StkType, push, pop, empty ) where
  newtype StkType a = Stk [a]
  push x (Stk s) = Stk (x:s)
  pop (Stk (_:s)) = Stk s
  empty = Stk []



data Bool = False | True deriving
  (Read, Show, Eq, Ord, Enum, Bounded)

data [a] = [] | a : [a] deriving Ord

data () = () deriving(Eq, Ord, Bounded, Enum, Read, Show)



23*36  -- single line comment
--     ^^ punctuation.definition.comment.haskell
--     ^^^^^^^^^^^^^^^^^^^^^^^ comment.line.double-dash.haskell
23*36

-- <- - comment.line.double-dash.haskell
--() <- - comment.line.double-dash.haskell

   --> xx
-- ^^^ keyword.operator.haskell
   &-- xx
-- ^^^ keyword.operator.haskell

   {- block comment -} 23*36
-- ^^ punctuation.definition.comment.begin.haskell
-- ^^^^^^^^^^^^^^^^^^^ comment.block.haskell
--                  ^^ punctuation.definition.comment.end.haskell
--                    ^ - comment.block.haskell

   {- {-# #-} -} 23*36
-- ^^ punctuation.definition.comment.begin.haskell
-- ^^^^^^^^^^^^^ comment.block.haskell - meta.preprocessor.haskell
--            ^^ punctuation.definition.comment.end.haskell
--              ^ - comment.block.haskell

   {- {- #-} -} 23*36
-- ^^ punctuation.definition.comment.begin.haskell
-- ^^^^^^^^^^^^ comment.block.haskell
--           ^^ punctuation.definition.comment.end.haskell
--             ^ - comment.block.haskell

   {- {- -} -} 23*36
-- ^^ punctuation.definition.comment.begin.haskell
-- ^^^^^^^^^^^ comment.block.haskell
--          ^^ punctuation.definition.comment.end.haskell
--            ^ - comment.block.haskell

   {- {-# -} -} 23*36
-- ^^ punctuation.definition.comment.begin.haskell
-- ^^^^^^^^^^^^ comment.block.haskell - meta.preprocessor.haskell
--           ^^ punctuation.definition.comment.end.haskell
--             ^ - comment.block.haskell

   {- {-# {- test -} -} -} 23*36
-- ^^ punctuation.definition.comment.begin.haskell
-- ^^^^^^^^^^^^^^^^^^^^^^^ comment.block.haskell - meta.preprocessor.haskell
--                      ^^ punctuation.definition.comment.end.haskell
--                        ^ - comment.block.haskell

   class (Func t, Foldable t) => Traversable t where
-- ^^^^^ keyword.other.haskell
--        ^^^^ entity.other.inherited-class.haskell
--             ^ variable.other.generic-type.haskell
--                ^^^^^^^^ support.class.prelude.haskell
--                            ^^ keyword.other.big-arrow.haskell
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.declaration.class.haskell
   {-# MINIMAL traverse | sequenceA LANGUAGE #-}
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.preprocessor.haskell
--                                              ^ - meta.preprocessor.haskell
--                                   ^^^^^^^ keyword.other.preprocessor.haskell
--     ^^^^^^^ keyword.other.preprocessor.haskell

-- | Map each element of a structure to an action,
-- evaluate these actions from left to right, and
-- collect the results. For a version that ignores
-- the results see 'Data.Foldable.traverse_'.
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ comment.line.double-dash.haskell

   traverse :: Applicative f =>
-- ^^^^^^^^ entity.name.function.haskell
--          ^^ keyword.other.double-colon.haskell
--             ^^^^^^^^^^^ storage.type.haskell
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.function.type-declaration.haskell
--                           ^^ keyword.other.big-arrow.haskell
    (a -> f b)
-- ^^^^^^^^^^^^ meta.function.type-declaration.haskell
--     ^^ keyword.other.arrow.haskell
    -> t a
-- ^^^^^^^^ meta.function.type-declaration.haskell
--  ^^ keyword.other.arrow.haskell
    -> f (t b)
-- ^^^^^^^^^^^^ meta.function.type-declaration.haskell
--  ^^ keyword.other.arrow.haskell
   traverse f = sequenceA . fmap f
--            ^ keyword.operator.haskell
--                        ^ keyword.operator.haskell

-- | Evaluate each action in the structure from
-- left to right, and collect the results.
-- For a version that ignores the results see
-- 'Data.Foldable.sequenceA_'.
   sequenceA ∷ Applicative f ⇒ t (f a) → f (t a)
-- ^^^^^^^^^ entity.name.function.haskell
--           ^ keyword.other.double-colon.haskell
--             ^^^^^^^^^^^ storage.type.haskell
--                           ^ keyword.other.big-arrow.haskell
--                                     ^ keyword.other.arrow.haskell
   sequenceA = traverse id
--           ^ keyword.operator.haskell

   (==>&) :: Parse a -> Parse b -> Parse b
-- ^^^^^^ entity.name.function.haskell
   p ==>& f = p ==> \_ -> f
--   ^^^^ keyword.operator.haskell

   max3 :: Int -> Int -> Int -> Int
   max3 x y z
        | (x <= z) && (y <= z) = z
--      ^ keyword.operator.haskell
        | (x <= y) && (z <= y) = y
--      ^ keyword.operator.haskell
        | (y <= x) && (z <= x) = z
--      ^ keyword.operator.haskell

   {-# LANGUAGE MagicHash #-}
   splitWith :: (Word8 -> Bool) -> ByteString -> [ByteString]
   splitWith _pred (PS _  _   0) = []
   splitWith pred_ (PS fp off len) = splitWith0 pred# off len fp
      where pred# c# = pred_ (W8# c#)


   f.g
   F.g
-- ^^^ indentify
   f..
--  ^^ keyword.operator
   F..
-- ^^  contant
--   ^ keyword.operator.qualified
   F.
-- 