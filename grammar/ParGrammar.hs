{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -XPartialTypeSignatures #-}
#endif
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParGrammar where
import AbsGrammar
import LexGrammar
import ErrM
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.8

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn4 :: (Integer) -> (HappyAbsSyn )
happyIn4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn ) -> (Integer)
happyOut4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut4 #-}
happyIn5 :: (String) -> (HappyAbsSyn )
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> (String)
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
happyIn6 :: (Ident) -> (HappyAbsSyn )
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> (Ident)
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyIn7 :: (Error) -> (HappyAbsSyn )
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> (Error)
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: (Program) -> (HappyAbsSyn )
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> (Program)
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: (Boolean) -> (HappyAbsSyn )
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> (Boolean)
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyIn10 :: (Map) -> (HappyAbsSyn )
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> (Map)
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyIn11 :: ([Map]) -> (HappyAbsSyn )
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> ([Map])
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
happyIn12 :: (Literal) -> (HappyAbsSyn )
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> (Literal)
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyIn13 :: (Comp) -> (HappyAbsSyn )
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> (Comp)
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
happyIn14 :: (BOp) -> (HappyAbsSyn )
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> (BOp)
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
happyIn15 :: (Exp) -> (HappyAbsSyn )
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> (Exp)
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
happyIn16 :: (Exp) -> (HappyAbsSyn )
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> (Exp)
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
happyIn17 :: (Exp) -> (HappyAbsSyn )
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> (Exp)
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
happyIn18 :: (Exp) -> (HappyAbsSyn )
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> (Exp)
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
happyIn19 :: (Exp) -> (HappyAbsSyn )
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> (Exp)
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
happyIn20 :: (Exp) -> (HappyAbsSyn )
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> (Exp)
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
happyIn21 :: (Exp) -> (HappyAbsSyn )
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> (Exp)
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
happyIn22 :: ([Exp]) -> (HappyAbsSyn )
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> ([Exp])
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
happyIn23 :: (Call) -> (HappyAbsSyn )
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> (Call)
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
happyIn24 :: (Type) -> (HappyAbsSyn )
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> (Type)
happyOut24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut24 #-}
happyIn25 :: ([Type]) -> (HappyAbsSyn )
happyIn25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> ([Type])
happyOut25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut25 #-}
happyIn26 :: (Ret) -> (HappyAbsSyn )
happyIn26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> (Ret)
happyOut26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut26 #-}
happyIn27 :: (RVal) -> (HappyAbsSyn )
happyIn27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> (RVal)
happyOut27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut27 #-}
happyIn28 :: (Param) -> (HappyAbsSyn )
happyIn28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> (Param)
happyOut28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut28 #-}
happyIn29 :: ([Param]) -> (HappyAbsSyn )
happyIn29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn ) -> ([Param])
happyOut29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut29 #-}
happyIn30 :: (Var) -> (HappyAbsSyn )
happyIn30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn ) -> (Var)
happyOut30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut30 #-}
happyIn31 :: ([Var]) -> (HappyAbsSyn )
happyIn31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn ) -> ([Var])
happyOut31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut31 #-}
happyIn32 :: (Decl) -> (HappyAbsSyn )
happyIn32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn ) -> (Decl)
happyOut32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut32 #-}
happyIn33 :: (Elif) -> (HappyAbsSyn )
happyIn33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn ) -> (Elif)
happyOut33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut33 #-}
happyIn34 :: ([Elif]) -> (HappyAbsSyn )
happyIn34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> ([Elif])
happyOut34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut34 #-}
happyIn35 :: (Else) -> (HappyAbsSyn )
happyIn35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn ) -> (Else)
happyOut35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut35 #-}
happyIn36 :: (Stmt) -> (HappyAbsSyn )
happyIn36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn ) -> (Stmt)
happyOut36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut36 #-}
happyIn37 :: ([Stmt]) -> (HappyAbsSyn )
happyIn37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn ) -> ([Stmt])
happyOut37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut37 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x86\x31\x05\x01\x00\x00\x00\x00\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x01\x20\x04\x10\x21\xf2\x00\x00\x00\x00\x80\x00\x10\x02\x88\x10\x79\x00\x00\x00\x00\x40\x00\x08\x05\x44\x88\x3c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x10\x00\x42\x00\x11\x22\x0f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x81\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x24\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x42\x00\x11\x22\x0f\x00\x00\x00\x00\x08\x00\x21\x80\x08\x91\x07\x00\x00\x00\x00\x04\x80\x10\x40\x84\xc8\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x20\x04\x10\x20\xf2\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x08\x01\x44\x88\x3c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x21\x42\x84\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x21\x80\x08\x91\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x02\x40\x08\x20\x42\xe4\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x11\x22\x84\x00\x00\x00\x00\x00\x00\x00\x84\x08\x11\x42\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x44\x88\x10\x02\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x39\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x10\x02\x88\x10\x79\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x42\x00\x01\x22\x0f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x08\x01\x04\x88\x3c\x00\x00\x00\x00\x20\x00\x84\x00\x02\x44\x1e\x00\x00\x00\x00\x10\x00\x42\x00\x01\x22\x0f\x00\x00\x00\x00\x08\x00\x21\x80\x00\x91\x07\x00\x00\x00\x00\x04\x80\x10\x40\x80\xc8\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x41\x00\x00\x00\x00\x00\x00\x00\x00\x00\x90\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x86\x31\x25\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x08\x01\x44\x88\x3c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x42\x00\x11\x22\x0f\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x80\x00\x10\x02\x88\x10\x79\x00\x00\x00\x00\x00\x00\x00\x60\x18\x53\x12\x00\x00\x00\x00\x00\x00\x84\x08\x11\x42\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x40\x08\x20\x42\xe4\x01\x00\x00\x00\x00\x01\x20\x04\x10\x21\xf2\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x22\x44\x08\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x84\x08\x11\x42\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x84\x08\x11\x52\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x08\x01\x44\x88\x3c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x61\x4c\x49\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x18\x53\x12\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\xc6\x94\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pProgram","Integer","String","Ident","Error","Program","Boolean","Map","ListMap","Literal","Comp","BOp","Exp6","Exp5","Exp4","Exp3","Exp2","Exp1","Exp","ListExp","Call","Type","ListType","Ret","RVal","Param","ListParam","Var","ListVar","Decl","Elif","ListElif","Else","Stmt","ListStmt","'%'","'('","')'","'*'","'+'","','","'-'","'->'","'.'","'/'","':'","';'","'<'","'<='","'<|'","'='","'=='","'>'","'>='","'['","']'","'_'","'and'","'bool'","'break'","'continue'","'elif'","'else'","'error'","'false'","'func'","'if'","'int'","'not'","'or'","'print'","'return'","'string'","'true'","'var'","'void'","'while'","'{'","'|>'","'}'","L_integ","L_quoted","L_ident","L_Error","%eof"]
        bit_start = st * 87
        bit_end = (st + 1) * 87
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..86]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x00\x00\xd8\xff\x00\x00\xde\xff\x47\x00\x49\x00\x00\x00\x0e\x00\x0c\x00\x00\x00\x19\x00\x00\x00\x00\x00\x0f\x00\x0a\x00\x0a\x00\xff\xff\x0f\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x49\x00\x00\x00\x00\x00\x00\x00\x00\x00\x07\x00\x13\x00\x4a\x00\x00\x00\xec\xff\x21\x00\x00\x00\x0a\x00\x0a\x00\x0a\x00\x00\x00\x15\x00\x00\x00\x0a\x00\x00\x00\x00\x00\xf5\xff\x00\x00\x00\x00\x00\x00\x00\x00\x28\x00\x35\x00\x00\x00\x0a\x00\x33\x00\x0a\x00\x33\x00\x53\x00\x58\x00\x56\x00\x00\x00\x00\x00\x00\x00\x3a\x00\x00\x00\x55\x00\xf5\xff\xf5\xff\x00\x00\x00\x00\x00\x00\x00\x00\xf5\xff\x6a\x00\x45\x00\x6f\x00\x4a\x00\x67\x00\x51\x00\x82\x00\x00\x00\x0a\x00\x00\x00\x00\x00\x15\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x15\x00\x15\x00\x15\x00\x15\x00\x15\x00\x00\x00\x00\x00\x00\x00\x07\x00\x07\x00\x13\x00\x00\x00\x6e\x00\x00\x00\x00\x00\x00\x00\x0a\x00\x00\x00\x0a\x00\x7e\x00\x76\x00\x86\x00\x63\x00\x0a\x00\x80\x00\xf5\xff\x8a\x00\x8e\x00\x00\x00\x0a\x00\x0a\x00\x91\x00\x00\x00\x8d\x00\x6c\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf5\xff\x00\x00\xf5\xff\x00\x00\x00\x00\x00\x00\x70\x00\x00\x00\x2e\x00\x00\x00\x39\x00\x00\x00\x00\x00\x77\x00\x00\x00\x00\x00\x00\x00\x0a\x00\x78\x00\x00\x00\x00\x00\x7b\x00\x00\x00\x92\x00\x00\x00\xa4\x00\x00\x00\xb6\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\xfe\xff\x00\x00\x00\x00\x00\x00\x34\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x95\x00\x73\x01\x87\x01\x73\x00\x9f\x00\x9b\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9e\x00\x00\x00\xa5\x00\x00\x00\x00\x00\xaf\x01\x0f\x01\x23\x01\x00\x00\x27\x02\x00\x00\xe7\x00\x00\x00\x00\x00\x9a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc3\x01\x08\x00\x37\x01\xa7\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x25\x00\x93\x00\x00\x00\x54\x00\xa1\x00\x00\x00\x00\x00\x00\x00\x00\x00\xac\x00\x00\x00\x00\x00\x00\x00\xaa\x00\x00\x00\x00\x00\x00\x00\x97\x00\x13\x02\x00\x00\x00\x00\x36\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x45\x02\x52\x02\x5f\x02\x6b\x02\x77\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x34\x00\x00\x00\x00\x00\x00\x00\xd7\x01\x00\x00\xfb\x00\x00\x00\x00\x00\x00\x00\x00\x00\xeb\x01\x34\x00\xad\x00\x00\x00\x00\x00\x00\x00\x4b\x01\x5f\x01\x00\x00\x00\x00\x00\x00\x29\x00\x00\x00\x9b\x00\x00\x00\x00\x00\x59\x00\x00\x00\xb1\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe8\xff\x00\x00\x1e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xff\x01\x00\x00\x00\x00\xa6\x00\x00\x00\xa9\x00\x34\x00\xb2\x00\x34\x00\x00\x00\x34\x00\x00\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xa9\xff\x00\x00\xfe\xff\x00\x00\xbc\xff\xbd\xff\xaa\xff\xbb\xff\x00\x00\xb1\xff\x00\x00\xac\xff\xab\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfc\xff\xf3\xff\xf1\xff\xe4\xff\xf0\xff\xf2\xff\xe3\xff\xde\xff\xdb\xff\xd9\xff\xd7\xff\xd5\xff\xd4\xff\x00\x00\xe5\xff\x00\x00\xd3\xff\xd3\xff\xf8\xff\x00\x00\xf9\xff\xf6\xff\xfd\xff\xfb\xff\x00\x00\xc3\xff\xb3\xff\xc2\xff\xb2\xff\x00\x00\x00\x00\xa8\xff\x00\x00\xbc\xff\xd3\xff\x00\x00\x00\x00\xd2\xff\x00\x00\xbd\xff\xba\xff\xad\xff\xc0\xff\xa9\xff\x00\x00\x00\x00\x00\x00\xcd\xff\xcc\xff\xce\xff\xcb\xff\x00\x00\xf5\xff\x00\x00\x00\x00\xd8\xff\x00\x00\x00\x00\x00\x00\xa9\xff\x00\x00\xe7\xff\xe6\xff\x00\x00\xec\xff\xea\xff\xe8\xff\xeb\xff\xe9\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe0\xff\xe1\xff\xdf\xff\xdc\xff\xdd\xff\xda\xff\xd6\xff\xbc\xff\xe2\xff\xee\xff\xef\xff\x00\x00\xed\xff\xf6\xff\x00\x00\x00\x00\xc7\xff\x00\x00\x00\x00\xbc\xff\x00\x00\xbf\xff\x00\x00\xd0\xff\xd3\xff\xd3\xff\x00\x00\xd1\xff\x00\x00\xc0\xff\xc1\xff\xb6\xff\xb9\xff\xc8\xff\x00\x00\xca\xff\x00\x00\xf4\xff\xf7\xff\xae\xff\x00\x00\xc6\xff\xb0\xff\xbe\xff\x00\x00\xcf\xff\xc4\xff\x00\x00\xc5\xff\xb5\xff\xaf\xff\x00\x00\x00\x00\xc9\xff\xa9\xff\x00\x00\xa9\xff\xbc\xff\xa9\xff\xbc\xff\xb4\xff\xbc\xff\xb8\xff\xb7\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x02\x00\x04\x00\x17\x00\x0f\x00\x1d\x00\x2e\x00\x1f\x00\x01\x00\x14\x00\x02\x00\x04\x00\x02\x00\x18\x00\x0f\x00\x23\x00\x32\x00\x0a\x00\x1d\x00\x14\x00\x06\x00\x16\x00\x21\x00\x02\x00\x05\x00\x0f\x00\x07\x00\x26\x00\x10\x00\x1e\x00\x14\x00\x21\x00\x2b\x00\x22\x00\x1a\x00\x1b\x00\x0f\x00\x0c\x00\x27\x00\x02\x00\x1e\x00\x14\x00\x2b\x00\x02\x00\x22\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x27\x00\x14\x00\x1e\x00\x16\x00\x2b\x00\x02\x00\x02\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x27\x00\x18\x00\x19\x00\x30\x00\x2b\x00\x18\x00\x19\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x13\x00\x0f\x00\x1b\x00\x1c\x00\x02\x00\x2b\x00\x14\x00\x1a\x00\x1b\x00\x1c\x00\x18\x00\x09\x00\x2b\x00\x20\x00\x02\x00\x1d\x00\x0d\x00\x0e\x00\x03\x00\x21\x00\x11\x00\x12\x00\x13\x00\x06\x00\x26\x00\x19\x00\x1a\x00\x29\x00\x30\x00\x2b\x00\x10\x00\x1f\x00\x20\x00\x14\x00\x15\x00\x30\x00\x24\x00\x25\x00\x14\x00\x15\x00\x28\x00\x06\x00\x2a\x00\x2d\x00\x00\x00\x01\x00\x02\x00\x03\x00\x30\x00\x05\x00\x32\x00\x0b\x00\x08\x00\x15\x00\x2c\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x03\x00\x13\x00\x19\x00\x1a\x00\x0b\x00\x17\x00\x15\x00\x06\x00\x1f\x00\x20\x00\x2c\x00\x06\x00\x03\x00\x24\x00\x25\x00\x03\x00\x08\x00\x28\x00\x02\x00\x2a\x00\x19\x00\x1a\x00\x2d\x00\x30\x00\x2d\x00\x30\x00\x1f\x00\x20\x00\x02\x00\x2b\x00\x2b\x00\x24\x00\x25\x00\x2b\x00\x09\x00\x28\x00\x02\x00\x2a\x00\x19\x00\x1a\x00\x2d\x00\x14\x00\x0a\x00\x30\x00\x1f\x00\x20\x00\x09\x00\x21\x00\x14\x00\x24\x00\x25\x00\x21\x00\x1e\x00\x28\x00\xff\xff\x2a\x00\x19\x00\x1a\x00\x2d\x00\x14\x00\x14\x00\x30\x00\x1f\x00\x20\x00\x14\x00\xff\xff\x21\x00\x24\x00\x25\x00\x21\x00\xff\xff\x28\x00\xff\xff\x2a\x00\x19\x00\x1a\x00\x2d\x00\xff\xff\x21\x00\x30\x00\x1f\x00\x20\x00\xff\xff\xff\xff\xff\xff\x24\x00\x25\x00\xff\xff\xff\xff\x28\x00\xff\xff\x2a\x00\xff\xff\xff\xff\x2d\x00\xff\xff\xff\xff\x30\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\x05\x00\x06\x00\x07\x00\x08\x00\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\xff\xff\x13\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\x05\x00\x06\x00\x07\x00\x08\x00\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\xff\xff\x13\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\x05\x00\xff\xff\xff\xff\x08\x00\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\x05\x00\xff\xff\xff\xff\x08\x00\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\x05\x00\xff\xff\xff\xff\x08\x00\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\x05\x00\xff\xff\xff\xff\x08\x00\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\x05\x00\xff\xff\xff\xff\x08\x00\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\x05\x00\xff\xff\xff\xff\x08\x00\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\xff\xff\x13\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\x05\x00\xff\xff\xff\xff\x08\x00\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\xff\xff\x13\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\x05\x00\xff\xff\xff\xff\x08\x00\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\xff\xff\x13\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\x05\x00\xff\xff\xff\xff\x08\x00\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\xff\xff\x13\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\x05\x00\xff\xff\xff\xff\x08\x00\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\xff\xff\x13\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\x05\x00\xff\xff\xff\xff\x08\x00\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\xff\xff\x13\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\x05\x00\xff\xff\xff\xff\x08\x00\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\xff\xff\x13\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\x05\x00\xff\xff\xff\xff\x08\x00\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\xff\xff\x13\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\x05\x00\xff\xff\xff\xff\x08\x00\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\xff\xff\xff\xff\xff\xff\x13\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\x05\x00\xff\xff\xff\xff\x08\x00\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x00\x00\x01\x00\x02\x00\x03\x00\x13\x00\x05\x00\xff\xff\xff\xff\x08\x00\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x13\x00\x05\x00\xff\xff\xff\xff\x08\x00\xff\xff\xff\xff\x0b\x00\x0c\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\x05\x00\x13\x00\xff\xff\x08\x00\xff\xff\xff\xff\x0b\x00\x0c\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\x05\x00\x13\x00\xff\xff\x08\x00\xff\xff\xff\xff\x0b\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\x05\x00\xff\xff\x13\x00\x08\x00\xff\xff\xff\xff\x0b\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\x05\x00\xff\xff\x13\x00\x08\x00\xff\xff\xff\xff\x0b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x13\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x23\x00\x03\x00\x51\x00\x41\x00\x8e\x00\x03\x00\x8f\x00\x5b\x00\x42\x00\x3a\x00\x5c\x00\x23\x00\x43\x00\x24\x00\x52\x00\xff\xff\x5d\x00\x44\x00\x25\x00\x35\x00\x2f\x00\x45\x00\x23\x00\x59\x00\x24\x00\x5a\x00\x46\x00\x34\x00\x26\x00\x25\x00\x04\x00\x47\x00\x27\x00\x07\x00\x3b\x00\x24\x00\x33\x00\x28\x00\x71\x00\x26\x00\x25\x00\x29\x00\x71\x00\x27\x00\x03\x00\x2a\x00\x14\x00\x2b\x00\x28\x00\x8b\x00\x26\x00\x8c\x00\x29\x00\x05\x00\x3e\x00\x03\x00\x2a\x00\x14\x00\x2b\x00\x28\x00\x72\x00\x73\x00\x14\x00\x29\x00\x72\x00\x88\x00\x03\x00\x2a\x00\x14\x00\x2b\x00\x06\x00\x41\x00\x91\x00\x92\x00\x36\x00\x4f\x00\x42\x00\x07\x00\x08\x00\x09\x00\x43\x00\x37\x00\x3f\x00\x0a\x00\x77\x00\x44\x00\x54\x00\x55\x00\x75\x00\x45\x00\x56\x00\x57\x00\x58\x00\x76\x00\x46\x00\x0c\x00\x0d\x00\x8e\x00\x14\x00\x47\x00\x70\x00\x0e\x00\x0f\x00\x6d\x00\x6e\x00\x14\x00\x10\x00\x11\x00\x6d\x00\x86\x00\x12\x00\x6b\x00\x13\x00\x6a\x00\x14\x00\x15\x00\x16\x00\x17\x00\x14\x00\x18\x00\xfa\xff\x69\x00\x19\x00\x68\x00\x67\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x2c\x00\x66\x00\x21\x00\x0c\x00\x0d\x00\x82\x00\x2d\x00\x81\x00\x80\x00\x0e\x00\x0f\x00\x7f\x00\x7b\x00\x7a\x00\x10\x00\x11\x00\x8b\x00\x8a\x00\x12\x00\x31\x00\x13\x00\x0c\x00\x0d\x00\x85\x00\x14\x00\x93\x00\x14\x00\x0e\x00\x0f\x00\x2b\x00\x96\x00\x94\x00\x10\x00\x11\x00\x98\x00\x52\x00\x12\x00\x37\x00\x13\x00\x0c\x00\x0d\x00\x7d\x00\x3f\x00\x4f\x00\x14\x00\x0e\x00\x0f\x00\x52\x00\x70\x00\x6c\x00\x10\x00\x11\x00\x64\x00\x87\x00\x12\x00\x00\x00\x13\x00\x0c\x00\x0d\x00\x9c\x00\x6b\x00\x7b\x00\x14\x00\x0e\x00\x0f\x00\x85\x00\x00\x00\x98\x00\x10\x00\x11\x00\x96\x00\x00\x00\x12\x00\x00\x00\x13\x00\x0c\x00\x0d\x00\x9a\x00\x00\x00\x9a\x00\x14\x00\x0e\x00\x0f\x00\x00\x00\x00\x00\x00\x00\x10\x00\x11\x00\x00\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x00\x00\x9d\x00\x00\x00\x00\x00\x14\x00\x14\x00\x15\x00\x16\x00\x17\x00\x00\x00\x18\x00\x47\x00\x48\x00\x19\x00\x00\x00\x00\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x49\x00\x00\x00\x21\x00\x14\x00\x15\x00\x16\x00\x17\x00\x00\x00\x18\x00\x47\x00\x82\x00\x19\x00\x00\x00\x00\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x49\x00\x00\x00\x21\x00\x14\x00\x15\x00\x16\x00\x17\x00\x00\x00\x18\x00\x00\x00\x00\x00\x19\x00\x00\x00\x00\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x38\x00\x4c\x00\x21\x00\x14\x00\x15\x00\x16\x00\x17\x00\x00\x00\x18\x00\x00\x00\x00\x00\x19\x00\x00\x00\x00\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x38\x00\x4b\x00\x21\x00\x14\x00\x15\x00\x16\x00\x17\x00\x00\x00\x18\x00\x00\x00\x00\x00\x19\x00\x00\x00\x00\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x38\x00\x39\x00\x21\x00\x14\x00\x15\x00\x16\x00\x17\x00\x00\x00\x18\x00\x00\x00\x00\x00\x19\x00\x00\x00\x00\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x38\x00\x78\x00\x21\x00\x14\x00\x15\x00\x16\x00\x17\x00\x00\x00\x18\x00\x00\x00\x00\x00\x19\x00\x00\x00\x00\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x38\x00\x77\x00\x21\x00\x14\x00\x15\x00\x16\x00\x17\x00\x00\x00\x18\x00\x00\x00\x00\x00\x19\x00\x00\x00\x00\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x30\x00\x00\x00\x21\x00\x14\x00\x15\x00\x16\x00\x17\x00\x00\x00\x18\x00\x00\x00\x00\x00\x19\x00\x00\x00\x00\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x2f\x00\x00\x00\x21\x00\x14\x00\x15\x00\x16\x00\x17\x00\x00\x00\x18\x00\x00\x00\x00\x00\x19\x00\x00\x00\x00\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x00\x00\x21\x00\x14\x00\x15\x00\x16\x00\x17\x00\x00\x00\x18\x00\x00\x00\x00\x00\x19\x00\x00\x00\x00\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x4d\x00\x00\x00\x21\x00\x14\x00\x15\x00\x16\x00\x17\x00\x00\x00\x18\x00\x00\x00\x00\x00\x19\x00\x00\x00\x00\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x3c\x00\x00\x00\x21\x00\x14\x00\x15\x00\x16\x00\x17\x00\x00\x00\x18\x00\x00\x00\x00\x00\x19\x00\x00\x00\x00\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x83\x00\x00\x00\x21\x00\x14\x00\x15\x00\x16\x00\x17\x00\x00\x00\x18\x00\x00\x00\x00\x00\x19\x00\x00\x00\x00\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x7d\x00\x00\x00\x21\x00\x14\x00\x15\x00\x16\x00\x17\x00\x00\x00\x18\x00\x00\x00\x00\x00\x19\x00\x00\x00\x00\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x94\x00\x00\x00\x21\x00\x14\x00\x15\x00\x16\x00\x17\x00\x00\x00\x18\x00\x00\x00\x00\x00\x19\x00\x00\x00\x00\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x63\x00\x00\x00\x00\x00\x00\x00\x21\x00\x14\x00\x15\x00\x16\x00\x17\x00\x00\x00\x18\x00\x00\x00\x00\x00\x19\x00\x00\x00\x00\x00\x1a\x00\x1b\x00\x1c\x00\x4a\x00\x14\x00\x15\x00\x16\x00\x17\x00\x21\x00\x18\x00\x00\x00\x00\x00\x19\x00\x00\x00\x00\x00\x1a\x00\x1b\x00\x62\x00\x00\x00\x14\x00\x15\x00\x16\x00\x17\x00\x21\x00\x18\x00\x00\x00\x00\x00\x19\x00\x00\x00\x00\x00\x1a\x00\x61\x00\x14\x00\x15\x00\x16\x00\x17\x00\x00\x00\x18\x00\x21\x00\x00\x00\x19\x00\x00\x00\x00\x00\x1a\x00\x60\x00\x14\x00\x15\x00\x16\x00\x17\x00\x00\x00\x18\x00\x21\x00\x00\x00\x19\x00\x00\x00\x00\x00\x5f\x00\x14\x00\x15\x00\x16\x00\x17\x00\x00\x00\x18\x00\x00\x00\x21\x00\x19\x00\x00\x00\x00\x00\x5e\x00\x14\x00\x15\x00\x16\x00\x17\x00\x00\x00\x18\x00\x00\x00\x21\x00\x19\x00\x00\x00\x00\x00\x5d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x21\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 87) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79),
	(80 , happyReduce_80),
	(81 , happyReduce_81),
	(82 , happyReduce_82),
	(83 , happyReduce_83),
	(84 , happyReduce_84),
	(85 , happyReduce_85),
	(86 , happyReduce_86),
	(87 , happyReduce_87)
	]

happy_n_terms = 51 :: Int
happy_n_nonterms = 34 :: Int

happyReduce_1 = happySpecReduce_1  0# happyReduction_1
happyReduction_1 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TI happy_var_1)) -> 
	happyIn4
		 ((read ( happy_var_1)) :: Integer
	)}

happyReduce_2 = happySpecReduce_1  1# happyReduction_2
happyReduction_2 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TL happy_var_1)) -> 
	happyIn5
		 (happy_var_1
	)}

happyReduce_3 = happySpecReduce_1  2# happyReduction_3
happyReduction_3 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TV happy_var_1)) -> 
	happyIn6
		 (Ident happy_var_1
	)}

happyReduce_4 = happySpecReduce_1  3# happyReduction_4
happyReduction_4 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (T_Error happy_var_1)) -> 
	happyIn7
		 (Error (happy_var_1)
	)}

happyReduce_5 = happySpecReduce_1  4# happyReduction_5
happyReduction_5 happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	happyIn8
		 (AbsGrammar.Prog (reverse happy_var_1)
	)}

happyReduce_6 = happySpecReduce_1  5# happyReduction_6
happyReduction_6 happy_x_1
	 =  happyIn9
		 (AbsGrammar.BTrue
	)

happyReduce_7 = happySpecReduce_1  5# happyReduction_7
happyReduction_7 happy_x_1
	 =  happyIn9
		 (AbsGrammar.BFalse
	)

happyReduce_8 = happySpecReduce_3  6# happyReduction_8
happyReduction_8 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	case happyOut21 happy_x_3 of { happy_var_3 -> 
	happyIn10
		 (AbsGrammar.MapKV happy_var_1 happy_var_3
	)}}

happyReduce_9 = happySpecReduce_0  7# happyReduction_9
happyReduction_9  =  happyIn11
		 ([]
	)

happyReduce_10 = happySpecReduce_1  7# happyReduction_10
happyReduction_10 happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	happyIn11
		 ((:[]) happy_var_1
	)}

happyReduce_11 = happySpecReduce_3  7# happyReduction_11
happyReduction_11 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	case happyOut11 happy_x_3 of { happy_var_3 -> 
	happyIn11
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_12 = happySpecReduce_1  8# happyReduction_12
happyReduction_12 happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	happyIn12
		 (AbsGrammar.LInt happy_var_1
	)}

happyReduce_13 = happySpecReduce_1  8# happyReduction_13
happyReduction_13 happy_x_1
	 =  case happyOut9 happy_x_1 of { happy_var_1 -> 
	happyIn12
		 (AbsGrammar.LBool happy_var_1
	)}

happyReduce_14 = happySpecReduce_1  8# happyReduction_14
happyReduction_14 happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	happyIn12
		 (AbsGrammar.LStr happy_var_1
	)}

happyReduce_15 = happySpecReduce_1  8# happyReduction_15
happyReduction_15 happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	happyIn12
		 (AbsGrammar.LErr happy_var_1
	)}

happyReduce_16 = happySpecReduce_3  8# happyReduction_16
happyReduction_16 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut22 happy_x_2 of { happy_var_2 -> 
	happyIn12
		 (AbsGrammar.LArr happy_var_2
	)}

happyReduce_17 = happySpecReduce_3  8# happyReduction_17
happyReduction_17 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut22 happy_x_2 of { happy_var_2 -> 
	happyIn12
		 (AbsGrammar.LTup happy_var_2
	)}

happyReduce_18 = happySpecReduce_3  8# happyReduction_18
happyReduction_18 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut11 happy_x_2 of { happy_var_2 -> 
	happyIn12
		 (AbsGrammar.LMap happy_var_2
	)}

happyReduce_19 = happySpecReduce_1  9# happyReduction_19
happyReduction_19 happy_x_1
	 =  happyIn13
		 (AbsGrammar.CLt
	)

happyReduce_20 = happySpecReduce_1  9# happyReduction_20
happyReduction_20 happy_x_1
	 =  happyIn13
		 (AbsGrammar.CGt
	)

happyReduce_21 = happySpecReduce_1  9# happyReduction_21
happyReduction_21 happy_x_1
	 =  happyIn13
		 (AbsGrammar.CLe
	)

happyReduce_22 = happySpecReduce_1  9# happyReduction_22
happyReduction_22 happy_x_1
	 =  happyIn13
		 (AbsGrammar.CGe
	)

happyReduce_23 = happySpecReduce_1  9# happyReduction_23
happyReduction_23 happy_x_1
	 =  happyIn13
		 (AbsGrammar.CEq
	)

happyReduce_24 = happySpecReduce_1  10# happyReduction_24
happyReduction_24 happy_x_1
	 =  happyIn14
		 (AbsGrammar.BAnd
	)

happyReduce_25 = happySpecReduce_1  10# happyReduction_25
happyReduction_25 happy_x_1
	 =  happyIn14
		 (AbsGrammar.BOr
	)

happyReduce_26 = happySpecReduce_1  11# happyReduction_26
happyReduction_26 happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	happyIn15
		 (AbsGrammar.ECall happy_var_1
	)}

happyReduce_27 = happySpecReduce_1  11# happyReduction_27
happyReduction_27 happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	happyIn15
		 (AbsGrammar.EVar happy_var_1
	)}

happyReduce_28 = happySpecReduce_1  11# happyReduction_28
happyReduction_28 happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	happyIn15
		 (AbsGrammar.ELit happy_var_1
	)}

happyReduce_29 = happySpecReduce_3  11# happyReduction_29
happyReduction_29 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut21 happy_x_2 of { happy_var_2 -> 
	happyIn15
		 (happy_var_2
	)}

happyReduce_30 = happySpecReduce_3  12# happyReduction_30
happyReduction_30 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut15 happy_x_3 of { happy_var_3 -> 
	happyIn16
		 (AbsGrammar.ETimes happy_var_1 happy_var_3
	)}}

happyReduce_31 = happySpecReduce_3  12# happyReduction_31
happyReduction_31 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut15 happy_x_3 of { happy_var_3 -> 
	happyIn16
		 (AbsGrammar.EDiv happy_var_1 happy_var_3
	)}}

happyReduce_32 = happySpecReduce_3  12# happyReduction_32
happyReduction_32 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut15 happy_x_3 of { happy_var_3 -> 
	happyIn16
		 (AbsGrammar.EMod happy_var_1 happy_var_3
	)}}

happyReduce_33 = happySpecReduce_1  12# happyReduction_33
happyReduction_33 happy_x_1
	 =  case happyOut15 happy_x_1 of { happy_var_1 -> 
	happyIn16
		 (happy_var_1
	)}

happyReduce_34 = happySpecReduce_3  13# happyReduction_34
happyReduction_34 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut17 happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_3 of { happy_var_3 -> 
	happyIn17
		 (AbsGrammar.EPlus happy_var_1 happy_var_3
	)}}

happyReduce_35 = happySpecReduce_3  13# happyReduction_35
happyReduction_35 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut17 happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_3 of { happy_var_3 -> 
	happyIn17
		 (AbsGrammar.EMinus happy_var_1 happy_var_3
	)}}

happyReduce_36 = happySpecReduce_1  13# happyReduction_36
happyReduction_36 happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	happyIn17
		 (happy_var_1
	)}

happyReduce_37 = happySpecReduce_3  14# happyReduction_37
happyReduction_37 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_2 of { happy_var_2 -> 
	case happyOut17 happy_x_3 of { happy_var_3 -> 
	happyIn18
		 (AbsGrammar.EComp happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_38 = happySpecReduce_1  14# happyReduction_38
happyReduction_38 happy_x_1
	 =  case happyOut17 happy_x_1 of { happy_var_1 -> 
	happyIn18
		 (happy_var_1
	)}

happyReduce_39 = happySpecReduce_2  15# happyReduction_39
happyReduction_39 happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_2 of { happy_var_2 -> 
	happyIn19
		 (AbsGrammar.ENot happy_var_2
	)}

happyReduce_40 = happySpecReduce_1  15# happyReduction_40
happyReduction_40 happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	happyIn19
		 (happy_var_1
	)}

happyReduce_41 = happySpecReduce_3  16# happyReduction_41
happyReduction_41 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut20 happy_x_1 of { happy_var_1 -> 
	case happyOut14 happy_x_2 of { happy_var_2 -> 
	case happyOut19 happy_x_3 of { happy_var_3 -> 
	happyIn20
		 (AbsGrammar.EBool happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_42 = happySpecReduce_1  16# happyReduction_42
happyReduction_42 happy_x_1
	 =  case happyOut19 happy_x_1 of { happy_var_1 -> 
	happyIn20
		 (happy_var_1
	)}

happyReduce_43 = happySpecReduce_1  17# happyReduction_43
happyReduction_43 happy_x_1
	 =  case happyOut20 happy_x_1 of { happy_var_1 -> 
	happyIn21
		 (happy_var_1
	)}

happyReduce_44 = happySpecReduce_0  18# happyReduction_44
happyReduction_44  =  happyIn22
		 ([]
	)

happyReduce_45 = happySpecReduce_1  18# happyReduction_45
happyReduction_45 happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	happyIn22
		 ((:[]) happy_var_1
	)}

happyReduce_46 = happySpecReduce_3  18# happyReduction_46
happyReduction_46 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	case happyOut22 happy_x_3 of { happy_var_3 -> 
	happyIn22
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_47 = happyReduce 4# 19# happyReduction_47
happyReduction_47 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut6 happy_x_1 of { happy_var_1 -> 
	case happyOut22 happy_x_3 of { happy_var_3 -> 
	happyIn23
		 (AbsGrammar.CFun happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_48 = happyReduce 6# 19# happyReduction_48
happyReduction_48 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut6 happy_x_1 of { happy_var_1 -> 
	case happyOut6 happy_x_3 of { happy_var_3 -> 
	case happyOut22 happy_x_5 of { happy_var_5 -> 
	happyIn23
		 (AbsGrammar.CMet happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_49 = happySpecReduce_1  20# happyReduction_49
happyReduction_49 happy_x_1
	 =  happyIn24
		 (AbsGrammar.TInt
	)

happyReduce_50 = happySpecReduce_1  20# happyReduction_50
happyReduction_50 happy_x_1
	 =  happyIn24
		 (AbsGrammar.TBool
	)

happyReduce_51 = happySpecReduce_1  20# happyReduction_51
happyReduction_51 happy_x_1
	 =  happyIn24
		 (AbsGrammar.TError
	)

happyReduce_52 = happySpecReduce_1  20# happyReduction_52
happyReduction_52 happy_x_1
	 =  happyIn24
		 (AbsGrammar.TString
	)

happyReduce_53 = happySpecReduce_3  20# happyReduction_53
happyReduction_53 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_2 of { happy_var_2 -> 
	happyIn24
		 (AbsGrammar.TArray happy_var_2
	)}

happyReduce_54 = happyReduce 5# 20# happyReduction_54
happyReduction_54 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut24 happy_x_2 of { happy_var_2 -> 
	case happyOut24 happy_x_4 of { happy_var_4 -> 
	happyIn24
		 (AbsGrammar.TMap happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_55 = happySpecReduce_3  20# happyReduction_55
happyReduction_55 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_2 of { happy_var_2 -> 
	happyIn24
		 (AbsGrammar.TTuple happy_var_2
	)}

happyReduce_56 = happySpecReduce_1  21# happyReduction_56
happyReduction_56 happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	happyIn25
		 ((:[]) happy_var_1
	)}

happyReduce_57 = happySpecReduce_3  21# happyReduction_57
happyReduction_57 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_3 of { happy_var_3 -> 
	happyIn25
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_58 = happySpecReduce_1  22# happyReduction_58
happyReduction_58 happy_x_1
	 =  happyIn26
		 (AbsGrammar.RVoid
	)

happyReduce_59 = happySpecReduce_1  22# happyReduction_59
happyReduction_59 happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	happyIn26
		 (AbsGrammar.RType happy_var_1
	)}

happyReduce_60 = happySpecReduce_1  23# happyReduction_60
happyReduction_60 happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	happyIn27
		 (AbsGrammar.RExp happy_var_1
	)}

happyReduce_61 = happySpecReduce_1  23# happyReduction_61
happyReduction_61 happy_x_1
	 =  happyIn27
		 (AbsGrammar.RNone
	)

happyReduce_62 = happySpecReduce_2  24# happyReduction_62
happyReduction_62 happy_x_2
	happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	case happyOut24 happy_x_2 of { happy_var_2 -> 
	happyIn28
		 (AbsGrammar.PVal happy_var_1 happy_var_2
	)}}

happyReduce_63 = happySpecReduce_0  25# happyReduction_63
happyReduction_63  =  happyIn29
		 ([]
	)

happyReduce_64 = happySpecReduce_1  25# happyReduction_64
happyReduction_64 happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	happyIn29
		 ((:[]) happy_var_1
	)}

happyReduce_65 = happySpecReduce_3  25# happyReduction_65
happyReduction_65 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	case happyOut29 happy_x_3 of { happy_var_3 -> 
	happyIn29
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_66 = happySpecReduce_1  26# happyReduction_66
happyReduction_66 happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	happyIn30
		 (AbsGrammar.AVar happy_var_1
	)}

happyReduce_67 = happySpecReduce_0  27# happyReduction_67
happyReduction_67  =  happyIn31
		 ([]
	)

happyReduce_68 = happySpecReduce_1  27# happyReduction_68
happyReduction_68 happy_x_1
	 =  case happyOut30 happy_x_1 of { happy_var_1 -> 
	happyIn31
		 ((:[]) happy_var_1
	)}

happyReduce_69 = happySpecReduce_3  27# happyReduction_69
happyReduction_69 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut30 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn31
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_70 = happyReduce 5# 28# happyReduction_70
happyReduction_70 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut6 happy_x_2 of { happy_var_2 -> 
	case happyOut24 happy_x_3 of { happy_var_3 -> 
	case happyOut21 happy_x_5 of { happy_var_5 -> 
	happyIn32
		 (AbsGrammar.DVar happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_71 = happyReduce 10# 28# happyReduction_71
happyReduction_71 (happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut6 happy_x_2 of { happy_var_2 -> 
	case happyOut29 happy_x_4 of { happy_var_4 -> 
	case happyOut26 happy_x_7 of { happy_var_7 -> 
	case happyOut37 happy_x_9 of { happy_var_9 -> 
	happyIn32
		 (AbsGrammar.DFunc happy_var_2 happy_var_4 happy_var_7 (reverse happy_var_9)
	) `HappyStk` happyRest}}}}

happyReduce_72 = happyReduce 5# 29# happyReduction_72
happyReduction_72 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut21 happy_x_2 of { happy_var_2 -> 
	case happyOut37 happy_x_4 of { happy_var_4 -> 
	happyIn33
		 (AbsGrammar.EElif happy_var_2 (reverse happy_var_4)
	) `HappyStk` happyRest}}

happyReduce_73 = happySpecReduce_0  30# happyReduction_73
happyReduction_73  =  happyIn34
		 ([]
	)

happyReduce_74 = happySpecReduce_2  30# happyReduction_74
happyReduction_74 happy_x_2
	happy_x_1
	 =  case happyOut34 happy_x_1 of { happy_var_1 -> 
	case happyOut33 happy_x_2 of { happy_var_2 -> 
	happyIn34
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_75 = happyReduce 4# 31# happyReduction_75
happyReduction_75 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut37 happy_x_3 of { happy_var_3 -> 
	happyIn35
		 (AbsGrammar.EElse (reverse happy_var_3)
	) `HappyStk` happyRest}

happyReduce_76 = happySpecReduce_2  32# happyReduction_76
happyReduction_76 happy_x_2
	happy_x_1
	 =  case happyOut27 happy_x_2 of { happy_var_2 -> 
	happyIn36
		 (AbsGrammar.SReturn happy_var_2
	)}

happyReduce_77 = happySpecReduce_2  32# happyReduction_77
happyReduction_77 happy_x_2
	happy_x_1
	 =  case happyOut21 happy_x_2 of { happy_var_2 -> 
	happyIn36
		 (AbsGrammar.SPrint happy_var_2
	)}

happyReduce_78 = happySpecReduce_1  32# happyReduction_78
happyReduction_78 happy_x_1
	 =  case happyOut32 happy_x_1 of { happy_var_1 -> 
	happyIn36
		 (AbsGrammar.SDecl happy_var_1
	)}

happyReduce_79 = happyReduce 6# 32# happyReduction_79
happyReduction_79 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut21 happy_x_2 of { happy_var_2 -> 
	case happyOut37 happy_x_4 of { happy_var_4 -> 
	case happyOut34 happy_x_6 of { happy_var_6 -> 
	happyIn36
		 (AbsGrammar.SIf happy_var_2 (reverse happy_var_4) (reverse happy_var_6)
	) `HappyStk` happyRest}}}

happyReduce_80 = happyReduce 7# 32# happyReduction_80
happyReduction_80 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut21 happy_x_2 of { happy_var_2 -> 
	case happyOut37 happy_x_4 of { happy_var_4 -> 
	case happyOut34 happy_x_6 of { happy_var_6 -> 
	case happyOut35 happy_x_7 of { happy_var_7 -> 
	happyIn36
		 (AbsGrammar.SIfelse happy_var_2 (reverse happy_var_4) (reverse happy_var_6) happy_var_7
	) `HappyStk` happyRest}}}}

happyReduce_81 = happyReduce 5# 32# happyReduction_81
happyReduction_81 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut21 happy_x_2 of { happy_var_2 -> 
	case happyOut37 happy_x_4 of { happy_var_4 -> 
	happyIn36
		 (AbsGrammar.SWhile happy_var_2 (reverse happy_var_4)
	) `HappyStk` happyRest}}

happyReduce_82 = happySpecReduce_3  32# happyReduction_82
happyReduction_82 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut21 happy_x_3 of { happy_var_3 -> 
	happyIn36
		 (AbsGrammar.SAssign happy_var_1 happy_var_3
	)}}

happyReduce_83 = happySpecReduce_1  32# happyReduction_83
happyReduction_83 happy_x_1
	 =  happyIn36
		 (AbsGrammar.SBreak
	)

happyReduce_84 = happySpecReduce_1  32# happyReduction_84
happyReduction_84 happy_x_1
	 =  happyIn36
		 (AbsGrammar.SCont
	)

happyReduce_85 = happySpecReduce_1  32# happyReduction_85
happyReduction_85 happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	happyIn36
		 (AbsGrammar.SCall happy_var_1
	)}

happyReduce_86 = happySpecReduce_0  33# happyReduction_86
happyReduction_86  =  happyIn37
		 ([]
	)

happyReduce_87 = happySpecReduce_3  33# happyReduction_87
happyReduction_87 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	case happyOut36 happy_x_2 of { happy_var_2 -> 
	happyIn37
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyNewToken action sts stk [] =
	happyDoAction 50# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 1#;
	PT _ (TS _ 2) -> cont 2#;
	PT _ (TS _ 3) -> cont 3#;
	PT _ (TS _ 4) -> cont 4#;
	PT _ (TS _ 5) -> cont 5#;
	PT _ (TS _ 6) -> cont 6#;
	PT _ (TS _ 7) -> cont 7#;
	PT _ (TS _ 8) -> cont 8#;
	PT _ (TS _ 9) -> cont 9#;
	PT _ (TS _ 10) -> cont 10#;
	PT _ (TS _ 11) -> cont 11#;
	PT _ (TS _ 12) -> cont 12#;
	PT _ (TS _ 13) -> cont 13#;
	PT _ (TS _ 14) -> cont 14#;
	PT _ (TS _ 15) -> cont 15#;
	PT _ (TS _ 16) -> cont 16#;
	PT _ (TS _ 17) -> cont 17#;
	PT _ (TS _ 18) -> cont 18#;
	PT _ (TS _ 19) -> cont 19#;
	PT _ (TS _ 20) -> cont 20#;
	PT _ (TS _ 21) -> cont 21#;
	PT _ (TS _ 22) -> cont 22#;
	PT _ (TS _ 23) -> cont 23#;
	PT _ (TS _ 24) -> cont 24#;
	PT _ (TS _ 25) -> cont 25#;
	PT _ (TS _ 26) -> cont 26#;
	PT _ (TS _ 27) -> cont 27#;
	PT _ (TS _ 28) -> cont 28#;
	PT _ (TS _ 29) -> cont 29#;
	PT _ (TS _ 30) -> cont 30#;
	PT _ (TS _ 31) -> cont 31#;
	PT _ (TS _ 32) -> cont 32#;
	PT _ (TS _ 33) -> cont 33#;
	PT _ (TS _ 34) -> cont 34#;
	PT _ (TS _ 35) -> cont 35#;
	PT _ (TS _ 36) -> cont 36#;
	PT _ (TS _ 37) -> cont 37#;
	PT _ (TS _ 38) -> cont 38#;
	PT _ (TS _ 39) -> cont 39#;
	PT _ (TS _ 40) -> cont 40#;
	PT _ (TS _ 41) -> cont 41#;
	PT _ (TS _ 42) -> cont 42#;
	PT _ (TS _ 43) -> cont 43#;
	PT _ (TS _ 44) -> cont 44#;
	PT _ (TS _ 45) -> cont 45#;
	PT _ (TI happy_dollar_dollar) -> cont 46#;
	PT _ (TL happy_dollar_dollar) -> cont 47#;
	PT _ (TV happy_dollar_dollar) -> cont 48#;
	PT _ (T_Error happy_dollar_dollar) -> cont 49#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 50# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => ([(Token)], [String]) -> Err a
happyError' = (\(tokens, _) -> happyError tokens)
pProgram tks = happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (happyOut8 x))

happySeq = happyDontSeq


returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}



















































































































































































-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 













-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif


data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList




















infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}
          

          case action of
                0#           -> {- nothing -}
                                     happyFail (happyExpListPerState ((Happy_GHC_Exts.I# (st)) :: Int)) i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}
                                                   
                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}
                                     

                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = happyAdjustOffset (indexShortOffAddr happyActOffsets st)
         off_i  = (off Happy_GHC_Exts.+#  i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st




indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#




{-# INLINE happyLt #-}
happyLt x y = LT(x,y)


readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (indexShortOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 4#))) (bit `mod` 16)
  where unbox_int (Happy_GHC_Exts.I# x) = x






data HappyAddr = HappyA# Happy_GHC_Exts.Addr#


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st1)
             off_i = (off Happy_GHC_Exts.+#  nt)
             new_state = indexShortOffAddr happyTable off_i




          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st)
         off_i = (off Happy_GHC_Exts.+#  nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ( (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.

