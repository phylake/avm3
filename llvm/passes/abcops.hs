module LLVM.Passes.AbcOps (abcT) where

import           Data.Word
import           LLVM.AbcOps
import           LLVM.Lang (Label)
import           LLVM.Passes.Branch
import qualified Abc.Def as Abc

abcT :: (Abc.IntIdx -> Abc.S32)            -- int resolution
     -> (Abc.UintIdx -> Abc.U32)           -- uint resolution
     -> (Abc.DoubleIdx -> Double)          -- double resolution
     -> (Abc.StringIdx -> String)          -- string resolution
     -> (Abc.MultinameIdx -> Maybe String) -- multiname resolution
     -> [(BranchPrim2, Abc.OpCode)]
     -> [(Label, [OpCode])]
abcT i u d s m = abcT2 . map (abcT1 i u d s m)

abcT2 :: [(BranchPrim2, [OpCode])]
      -> [(Label, [OpCode])]
abcT2 ((DestP2 l, op):ops) = (l, op ++ labelOps ops):abcT2 ops
abcT2 ((ConditionalP2 _ _, _):ops) = abcT2 ops
abcT2 ((JumpP2 _, _):ops) = abcT2 ops
abcT2 ((NoPrim2, _):ops) = abcT2 ops
abcT2 [] = []

-- the op codes under a Label
labelOps :: [(BranchPrim2, [OpCode])] -> [OpCode]
labelOps ((ConditionalP2 _ _, op):ops) = op ++ labelOps ops
labelOps ((JumpP2 _, op):ops) = op ++ labelOps ops
labelOps ((NoPrim2, op):ops) = op ++ labelOps ops
labelOps ((DestP2 _, op):ops) = []
labelOps [] = []

abcT1 :: (Abc.IntIdx -> Abc.S32)            -- int resolution
      -> (Abc.UintIdx -> Abc.U32)           -- uint resolution
      -> (Abc.DoubleIdx -> Double)          -- double resolution
      -> (Abc.StringIdx -> String)          -- string resolution
      -> (Abc.MultinameIdx -> Maybe String) -- multiname resolution
      -> (BranchPrim2, Abc.OpCode)
      -> (BranchPrim2, [OpCode])
abcT1 {- 0x01 -} i u d s m (br, Abc.Breakpoint) = (br, [Breakpoint])
abcT1 {- 0x02 -} i u d s m (br, Abc.Nop) = (br, [Nop])
abcT1 {- 0x03 -} i u d s m (br, Abc.Throw) = (br, [Throw])
abcT1 {- 0x04 -} i u d s m (br, Abc.GetSuper u30) = (br, [GetSuper u30 $ m u30])
abcT1 {- 0x05 -} i u d s m (br, Abc.SetSuper u30) = (br, [SetSuper u30 $ m u30])
abcT1 {- 0x06 -} i u d s m (br, Abc.DefaultXmlNamespace u30) = (br, [DefaultXmlNamespace u30])
abcT1 {- 0x07 -} i u d s m (br, Abc.DefaultXmlNamespaceL) = (br, [DefaultXmlNamespaceL])
abcT1 {- 0x08 -} i u d s m (br, Abc.Kill u30) = (br, [Kill u30])
abcT1 {- 0x09 -} i u d s m (br, Abc.Label) =(br,  [])
abcT1 {- 0x0C -} i u d s m (br@(ConditionalP2 t f), Abc.IfNotLessThan _) = (br, [IfNotLessThan t f])
abcT1 {- 0x0D -} i u d s m (br@(ConditionalP2 t f), Abc.IfNotLessEqual _) = (br, [IfNotLessEqual t f])
abcT1 {- 0x0E -} i u d s m (br@(ConditionalP2 t f), Abc.IfNotGreaterThan _) = (br, [IfNotGreaterThan t f])
abcT1 {- 0x0F -} i u d s m (br@(ConditionalP2 t f), Abc.IfNotGreaterEqual _) = (br, [IfNotGreaterEqual t f])
abcT1 {- 0x10 -} i u d s m (br@(JumpP2 l), Abc.Jump _) = (br, [Jump l])
abcT1 {- 0x11 -} i u d s m (br@(ConditionalP2 t f), Abc.IfTrue _) = (br, [IfTrue t f])
abcT1 {- 0x12 -} i u d s m (br@(ConditionalP2 t f), Abc.IfFalse _) = (br, [IfFalse t f])
abcT1 {- 0x13 -} i u d s m (br@(ConditionalP2 t f), Abc.IfEqual _) = (br, [IfEqual t f])
abcT1 {- 0x14 -} i u d s m (br@(ConditionalP2 t f), Abc.IfNotEqual _) = (br, [IfNotEqual t f])
abcT1 {- 0x15 -} i u d s m (br@(ConditionalP2 t f), Abc.IfLessThan _) = (br, [IfLessThan t f])
abcT1 {- 0x16 -} i u d s m (br@(ConditionalP2 t f), Abc.IfLessEqual _) = (br, [IfLessEqual t f])
abcT1 {- 0x17 -} i u d s m (br@(ConditionalP2 t f), Abc.IfGreaterThan _) = (br, [IfGreaterThan t f])
abcT1 {- 0x18 -} i u d s m (br@(ConditionalP2 t f), Abc.IfGreaterEqual _) = (br, [IfGreaterEqual t f])
abcT1 {- 0x19 -} i u d s m (br@(ConditionalP2 t f), Abc.IfStrictEqual _) = (br, [IfStrictEqual t f])
abcT1 {- 0x1A -} i u d s m (br@(ConditionalP2 t f), Abc.IfStrictNotEqual _) = (br, [IfStrictNotEqual t f])
abcT1 {- 0x1B -} i u d s m (br, Abc.LookupSwitch _ _) =(br,  [])
abcT1 {- 0x1C -} i u d s m (br, Abc.PushWith) = (br, [PushWith])
abcT1 {- 0x1D -} i u d s m (br, Abc.PopScope) = (br, [PopScope])
abcT1 {- 0x1E -} i u d s m (br, Abc.NextName) = (br, [NextName])
abcT1 {- 0x1F -} i u d s m (br, Abc.HasNext) = (br, [HasNext])
abcT1 {- 0x20 -} i u d s m (br, Abc.PushNull) = (br, [PushNull])
abcT1 {- 0x21 -} i u d s m (br, Abc.PushUndefined) = (br, [PushUndefined])
abcT1 {- 0x22 -} i u d s m (br, Abc.PushConstant) = (br, [PushConstant])
abcT1 {- 0x23 -} i u d s m (br, Abc.NextValue) = (br, [NextValue])
abcT1 {- 0x24 -} i u d s m (br, Abc.PushByte u8) = (br, [PushByte u8])
abcT1 {- 0x25 -} i u d s m (br, Abc.PushShort u30) = (br, [PushShort u30])
abcT1 {- 0x26 -} i u d s m (br, Abc.PushTrue) = (br, [PushTrue])
abcT1 {- 0x27 -} i u d s m (br, Abc.PushFalse) = (br, [PushFalse])
abcT1 {- 0x28 -} i u d s m (br, Abc.PushNaN) = (br, [PushNaN])
abcT1 {- 0x29 -} i u d s m (br, Abc.Pop) = (br, [Pop])
abcT1 {- 0x2A -} i u d s m (br, Abc.Dup) = (br, [Dup])
abcT1 {- 0x2B -} i u d s m (br, Abc.Swap) = (br, [Swap])
abcT1 {- 0x2C -} i u d s m (br, Abc.PushString u30) = (br, [PushString $ s u30])
abcT1 {- 0x2D -} i u d s m (br, Abc.PushInt u30) = (br, [PushInt $ i u30])
abcT1 {- 0x2E -} i u d s m (br, Abc.PushUInt u30) = (br, [PushUInt $ u u30])
abcT1 {- 0x2F -} i u d s m (br, Abc.PushDouble u30) = (br, [PushDouble $ d u30])
abcT1 {- 0x30 -} i u d s m (br, Abc.PushScope) = (br, [PushScope])
abcT1 {- 0x31 -} i u d s m (br, Abc.PushNamespace u30) = (br, [PushNamespace u30])
abcT1 {- 0x32 -} i u d s m (br, Abc.HasNext2 w32 w32_2) = (br, [HasNext2 w32 w32_2])
abcT1 {- 0x33 -} i u d s m (br, Abc.PushDecimal) = (br, [PushDecimal])
abcT1 {- 0x34 -} i u d s m (br, Abc.PushDNaN) = (br, [PushDNaN])
abcT1 {- 0x40 -} i u d s m (br, Abc.NewFunction u30) = (br, [NewFunction u30])
abcT1 {- 0x41 -} i u d s m (br, Abc.Call u30) = (br, [Call u30])
abcT1 {- 0x42 -} i u d s m (br, Abc.Construct u30) = (br, [Construct u30])
abcT1 {- 0x43 -} i u d s m (br, Abc.CallMethod u30_1 u30_2) = (br, [CallMethod u30_1 u30_2 $ m u30_1])
abcT1 {- 0x44 -} i u d s m (br, Abc.CallStatic u30_1 u30_2) = (br, [CallStatic u30_1 u30_2 $ m u30_1])
abcT1 {- 0x45 -} i u d s m (br, Abc.CallSuper u30_1 u30_2) = (br, [CallSuper u30_1 u30_2 $ m u30_1])
abcT1 {- 0x46 -} i u d s m (br, Abc.CallProperty u30_1 u30_2) = (br, [CallProperty u30_1 u30_2 $ m u30_1])
abcT1 {- 0x47 -} i u d s m (br, Abc.ReturnVoid) = (br, [ReturnVoid])
abcT1 {- 0x48 -} i u d s m (br, Abc.ReturnValue) = (br, [ReturnValue])
abcT1 {- 0x49 -} i u d s m (br, Abc.ConstructSuper u30) = (br, [ConstructSuper u30])
abcT1 {- 0x4A -} i u d s m (br, Abc.ConstructProp u30_1 u30_2) = (br, [ConstructProp u30_1 u30_2 $ m u30_1])
abcT1 {- 0x4B -} i u d s m (br, Abc.CallSuperId) = (br, [CallSuperId])
abcT1 {- 0x4C -} i u d s m (br, Abc.CallPropLex u30_1 u30_2) = (br, [CallPropLex u30_1 u30_2 $ m u30_1])
abcT1 {- 0x4D -} i u d s m (br, Abc.CallInterface) = (br, [CallInterface])
abcT1 {- 0x4E -} i u d s m (br, Abc.CallSuperVoid u30_1 u30_2) = (br, [CallSuperVoid u30_1 u30_2 $ m u30_1])
abcT1 {- 0x4F -} i u d s m (br, Abc.CallPropVoid u30_1 u30_2) = (br, [CallPropVoid u30_1 u30_2 $ m u30_1])
abcT1 {- 0x53 -} i u d s m (br, Abc.ApplyType) = (br, [ApplyType])
abcT1 {- 0x55 -} i u d s m (br, Abc.NewObject u30) = (br, [NewObject u30])
abcT1 {- 0x56 -} i u d s m (br, Abc.NewArray u30) = (br, [NewArray u30])
abcT1 {- 0x57 -} i u d s m (br, Abc.NewActivation) = (br, [NewActivation])
abcT1 {- 0x58 -} i u d s m (br, Abc.NewClass u30) = (br, [NewClass u30])
abcT1 {- 0x59 -} i u d s m (br, Abc.GetDescendants u30) = (br, [GetDescendants u30 $ m u30])
abcT1 {- 0x5A -} i u d s m (br, Abc.NewCatch u30) = (br, [NewCatch u30])
abcT1 {- 0x5B -} i u d s m (br, Abc.FindPropGlobalStrict) = (br, [FindPropGlobalStrict])
abcT1 {- 0x5C -} i u d s m (br, Abc.FindPropGlobal) = (br, [FindPropGlobal])
abcT1 {- 0x5D -} i u d s m (br, Abc.FindPropStrict u30) = (br, [FindPropStrict u30 $ m u30])
abcT1 {- 0x5E -} i u d s m (br, Abc.FindProperty u30) = (br, [FindProperty u30 $ m u30])
abcT1 {- 0x5F -} i u d s m (br, Abc.FindDef) = (br, [FindDef])
abcT1 {- 0x60 -} i u d s m (br, Abc.GetLex idx) = (br, [FindPropStrict idx$ m idx, GetProperty idx$ m idx])
abcT1 {- 0x61 -} i u d s m (br, Abc.SetProperty u30) = (br, [SetProperty u30 $ m u30])
abcT1 {- 0x62 -} i u d s m (br, Abc.GetLocal u30) = (br, [GetLocal u30])
abcT1 {- 0x63 -} i u d s m (br, Abc.SetLocal u30) = (br, [SetLocal u30])
abcT1 {- 0x64 -} i u d s m (br, Abc.GetGlobalScope) = (br, [GetGlobalScope])
abcT1 {- 0x65 -} i u d s m (br, Abc.GetScopeObject u8) = (br, [GetScopeObject u8])
abcT1 {- 0x66 -} i u d s m (br, Abc.GetProperty u30) = (br, [GetProperty u30 $ m u30])
abcT1 {- 0x67 -} i u d s m (br, Abc.GetPropertyLate) = (br, [GetPropertyLate])
abcT1 {- 0x68 -} i u d s m (br, Abc.InitProperty u30) = (br, [InitProperty u30 $ m u30])
abcT1 {- 0x69 -} i u d s m (br, Abc.SetPropertyLate) = (br, [SetPropertyLate])
abcT1 {- 0x6A -} i u d s m (br, Abc.DeleteProperty u30) = (br, [DeleteProperty u30 $ m u30])
abcT1 {- 0x6B -} i u d s m (br, Abc.DeletePropertyLate) = (br, [DeletePropertyLate])
abcT1 {- 0x6C -} i u d s m (br, Abc.GetSlot u30) = (br, [GetSlot u30{- $ t u30-}])
abcT1 {- 0x6D -} i u d s m (br, Abc.SetSlot u30) = (br, [SetSlot u30{- $ t u30-}])
abcT1 {- 0x6E -} i u d s m (br, Abc.GetGlobalSlot u30) = (br, [GetGlobalSlot u30])
abcT1 {- 0x6F -} i u d s m (br, Abc.SetGlobalSlot u30) = (br, [SetGlobalSlot u30])
abcT1 {- 0x70 -} i u d s m (br, Abc.ConvertString) = (br, [ConvertString])
abcT1 {- 0x71 -} i u d s m (br, Abc.EscXmlElem) = (br, [EscXmlElem])
abcT1 {- 0x72 -} i u d s m (br, Abc.EscXmlAttr) = (br, [EscXmlAttr])
abcT1 {- 0x73 -} i u d s m (br, Abc.ConvertInt) = (br, [ConvertInt])
abcT1 {- 0x74 -} i u d s m (br, Abc.ConvertUInt) = (br, [ConvertUInt])
abcT1 {- 0x75 -} i u d s m (br, Abc.ConvertDouble) = (br, [ConvertDouble])
abcT1 {- 0x76 -} i u d s m (br, Abc.ConvertBoolean) = (br, [ConvertBoolean])
abcT1 {- 0x77 -} i u d s m (br, Abc.ConvertObject) = (br, [ConvertObject])
abcT1 {- 0x78 -} i u d s m (br, Abc.CheckFilter) = (br, [CheckFilter])
abcT1 {- 0x80 -} i u d s m (br, Abc.Coerce u30) = (br, [Coerce u30 $ m u30])
abcT1 {- 0x81 -} i u d s m (br, Abc.CoerceBoolean) = (br, [CoerceBoolean])
abcT1 {- 0x82 -} i u d s m (br, Abc.CoerceAny) = (br, [CoerceAny])
abcT1 {- 0x83 -} i u d s m (br, Abc.CoerceInt) = (br, [CoerceInt])
abcT1 {- 0x84 -} i u d s m (br, Abc.CoerceDouble) = (br, [CoerceDouble])
abcT1 {- 0x85 -} i u d s m (br, Abc.CoerceString) = (br, [CoerceString])
abcT1 {- 0x86 -} i u d s m (br, Abc.AsType u30) = (br, [AsType u30 $ m u30])
abcT1 {- 0x87 -} i u d s m (br, Abc.AsTypeLate) = (br, [AsTypeLate])
abcT1 {- 0x88 -} i u d s m (br, Abc.CoerceUInt) = (br, [CoerceUInt])
abcT1 {- 0x89 -} i u d s m (br, Abc.CoerceObject) = (br, [CoerceObject])
abcT1 {- 0x90 -} i u d s m (br, Abc.Negate) = (br, [Negate])
abcT1 {- 0x91 -} i u d s m (br, Abc.Increment) = (br, [Increment])
abcT1 {- 0x92 -} i u d s m (br, Abc.IncLocal u30) = (br, [IncLocal u30])
abcT1 {- 0x93 -} i u d s m (br, Abc.Decrement) = (br, [Decrement])
abcT1 {- 0x94 -} i u d s m (br, Abc.DecLocal u30) = (br, [DecLocal u30])
abcT1 {- 0x95 -} i u d s m (br, Abc.TypeOf) = (br, [TypeOf])
abcT1 {- 0x96 -} i u d s m (br, Abc.Not) = (br, [Not])
abcT1 {- 0x97 -} i u d s m (br, Abc.BitNot) = (br, [BitNot])
abcT1 {- 0x9A -} i u d s m (br, Abc.Concat) = (br, [Concat])
abcT1 {- 0x9B -} i u d s m (br, Abc.AddDouble) = (br, [AddDouble])
abcT1 {- 0xA0 -} i u d s m (br, Abc.Add) = (br, [Add])
abcT1 {- 0xA1 -} i u d s m (br, Abc.Subtract) = (br, [Subtract])
abcT1 {- 0xA2 -} i u d s m (br, Abc.Multiply) = (br, [Multiply])
abcT1 {- 0xA3 -} i u d s m (br, Abc.Divide) = (br, [Divide])
abcT1 {- 0xA4 -} i u d s m (br, Abc.Modulo) = (br, [Modulo])
abcT1 {- 0xA5 -} i u d s m (br, Abc.ShiftLeft) = (br, [ShiftLeft])
abcT1 {- 0xA6 -} i u d s m (br, Abc.ShiftRight) = (br, [ShiftRight])
abcT1 {- 0xA7 -} i u d s m (br, Abc.ShiftRightUnsigned) = (br, [ShiftRightUnsigned])
abcT1 {- 0xA8 -} i u d s m (br, Abc.BitAnd) = (br, [BitAnd])
abcT1 {- 0xA9 -} i u d s m (br, Abc.BitOr) = (br, [BitOr])
abcT1 {- 0xAA -} i u d s m (br, Abc.BitXor) = (br, [BitXor])
abcT1 {- 0xAB -} i u d s m (br, Abc.Equals) = (br, [Equals])
abcT1 {- 0xAC -} i u d s m (br, Abc.StrictEquals) = (br, [StrictEquals])
abcT1 {- 0xAD -} i u d s m (br, Abc.LessThan) = (br, [LessThan])
abcT1 {- 0xAE -} i u d s m (br, Abc.LessEquals) = (br, [LessEquals])
abcT1 {- 0xAF -} i u d s m (br, Abc.GreaterThan) = (br, [GreaterThan])
abcT1 {- 0xB0 -} i u d s m (br, Abc.GreaterEquals) = (br, [GreaterEquals])
abcT1 {- 0xB1 -} i u d s m (br, Abc.InstanceOf) = (br, [InstanceOf])
abcT1 {- 0xB2 -} i u d s m (br, Abc.IsType u30) = (br, [IsType u30 $ m u30])
abcT1 {- 0xB3 -} i u d s m (br, Abc.IsTypeLate) = (br, [IsTypeLate])
abcT1 {- 0xB4 -} i u d s m (br, Abc.In) = (br, [In])
abcT1 {- 0xC0 -} i u d s m (br, Abc.IncrementInt) = (br, [IncrementInt])
abcT1 {- 0xC1 -} i u d s m (br, Abc.DecrementInt) = (br, [DecrementInt])
abcT1 {- 0xC2 -} i u d s m (br, Abc.IncLocalInt u30) = (br, [GetLocal u30, IncrementInt, SetLocal u30])
abcT1 {- 0xC3 -} i u d s m (br, Abc.DecLocalInt u30) = (br, [GetLocal u30, DecrementInt, SetLocal u30])
abcT1 {- 0xC4 -} i u d s m (br, Abc.NegateInt) = (br, [NegateInt])
abcT1 {- 0xC5 -} i u d s m (br, Abc.AddInt) = (br, [AddInt])
abcT1 {- 0xC6 -} i u d s m (br, Abc.SubtractInt) = (br, [SubtractInt])
abcT1 {- 0xC7 -} i u d s m (br, Abc.MultiplyInt) = (br, [MultiplyInt])
abcT1 {- 0xD0 -} i u d s m (br, Abc.GetLocal0) = (br, [GetLocal 0])
abcT1 {- 0xD1 -} i u d s m (br, Abc.GetLocal1) = (br, [GetLocal 1])
abcT1 {- 0xD2 -} i u d s m (br, Abc.GetLocal2) = (br, [GetLocal 2])
abcT1 {- 0xD3 -} i u d s m (br, Abc.GetLocal3) = (br, [GetLocal 3])
abcT1 {- 0xD4 -} i u d s m (br, Abc.SetLocal0) = (br, [SetLocal 0])
abcT1 {- 0xD5 -} i u d s m (br, Abc.SetLocal1) = (br, [SetLocal 1])
abcT1 {- 0xD6 -} i u d s m (br, Abc.SetLocal2) = (br, [SetLocal 2])
abcT1 {- 0xD7 -} i u d s m (br, Abc.SetLocal3) = (br, [SetLocal 3])
abcT1 {- 0xEF -} i u d s m (br, Abc.Debug u8_1 u30_1 u8_2 u30_2) = (br, [Debug u8_1 u30_1 u8_2 u30_2])
abcT1 {- 0xF0 -} i u d s m (br, Abc.DebugLine u30) = (br, [DebugLine u30])
abcT1 {- 0xF1 -} i u d s m (br, Abc.DebugFile u30) = (br, [DebugFile u30])
abcT1 {- 0xF2 -} i u d s m (br, Abc.BreakpointLine) = (br, [BreakpointLine])
abcT1            i u d s m (br, _) = (br, [])
