/home/vagrant/ghc-dev/inplace/lib/bin/haddock -B/home/vagrant/ghc-dev/inplace/lib -l/home/vagrant/ghc-dev/inplace/lib --odir=compiler/stage2/doc/html/ghc --no-tmp-comp-dir --dump-interface=compiler/stage2/doc/html/ghc/ghc.haddock --html --hoogle --title=ghc-7.7.20130829: The GHC API --prologue=compiler/stage2/haddock-prologue.txt --read-interface=../Cabal-1.18.0,../Cabal-1.18.0/src/%{MODULE/./-}.html#%{NAME},libraries/Cabal/Cabal/dist-install/doc/html/Cabal/Cabal.haddock --read-interface=../array-0.4.0.2,../array-0.4.0.2/src/%{MODULE/./-}.html#%{NAME},libraries/array/dist-install/doc/html/array/array.haddock --read-interface=../base-4.7.0.0,../base-4.7.0.0/src/%{MODULE/./-}.html#%{NAME},libraries/base/dist-install/doc/html/base/base.haddock --read-interface=../bin-package-db-0.0.0.0,../bin-package-db-0.0.0.0/src/%{MODULE/./-}.html#%{NAME},libraries/bin-package-db/dist-install/doc/html/bin-package-db/bin-package-db.haddock --read-interface=../bytestring-0.10.3.0,../bytestring-0.10.3.0/src/%{MODULE/./-}.html#%{NAME},libraries/bytestring/dist-install/doc/html/bytestring/bytestring.haddock --read-interface=../containers-0.5.0.0,../containers-0.5.0.0/src/%{MODULE/./-}.html#%{NAME},libraries/containers/dist-install/doc/html/containers/containers.haddock --read-interface=../directory-1.2.0.1,../directory-1.2.0.1/src/%{MODULE/./-}.html#%{NAME},libraries/directory/dist-install/doc/html/directory/directory.haddock --read-interface=../filepath-1.3.0.2,../filepath-1.3.0.2/src/%{MODULE/./-}.html#%{NAME},libraries/filepath/dist-install/doc/html/filepath/filepath.haddock --read-interface=../hoopl-3.10.0.0,../hoopl-3.10.0.0/src/%{MODULE/./-}.html#%{NAME},libraries/hoopl/dist-install/doc/html/hoopl/hoopl.haddock --read-interface=../hpc-0.6.0.1,../hpc-0.6.0.1/src/%{MODULE/./-}.html#%{NAME},libraries/hpc/dist-install/doc/html/hpc/hpc.haddock --read-interface=../process-1.2.0.0,../process-1.2.0.0/src/%{MODULE/./-}.html#%{NAME},libraries/process/dist-install/doc/html/process/process.haddock --read-interface=../template-haskell-2.9.0.0,../template-haskell-2.9.0.0/src/%{MODULE/./-}.html#%{NAME},libraries/template-haskell/dist-install/doc/html/template-haskell/template-haskell.haddock --read-interface=../time-1.4.0.2,../time-1.4.0.2/src/%{MODULE/./-}.html#%{NAME},libraries/time/dist-install/doc/html/time/time.haddock --read-interface=../transformers-0.3.0.0,../transformers-0.3.0.0/src/%{MODULE/./-}.html#%{NAME},libraries/transformers/dist-install/doc/html/transformers/transformers.haddock --read-interface=../unix-2.7.0.0,../unix-2.7.0.0/src/%{MODULE/./-}.html#%{NAME},libraries/unix/dist-install/doc/html/unix/unix.haddock --optghc=-hisuf --optghc=dyn_hi --optghc=-osuf --optghc=dyn_o --optghc=-hcsuf --optghc=dyn_hc --optghc=-fPIC --optghc=-dynamic --optghc=-H64m --optghc=-O0 --optghc=-fasm --optghc=-package-name --optghc=ghc-7.7.20130829 --optghc=-hide-all-packages --optghc=-i --optghc=-icompiler/basicTypes --optghc=-icompiler/cmm --optghc=-icompiler/codeGen --optghc=-icompiler/coreSyn --optghc=-icompiler/deSugar --optghc=-icompiler/ghci --optghc=-icompiler/hsSyn --optghc=-icompiler/iface --optghc=-icompiler/llvmGen --optghc=-icompiler/main --optghc=-icompiler/nativeGen --optghc=-icompiler/parser --optghc=-icompiler/prelude --optghc=-icompiler/profiling --optghc=-icompiler/rename --optghc=-icompiler/simplCore --optghc=-icompiler/simplStg --optghc=-icompiler/specialise --optghc=-icompiler/stgSyn --optghc=-icompiler/stranal --optghc=-icompiler/typecheck --optghc=-icompiler/types --optghc=-icompiler/utils --optghc=-icompiler/vectorise --optghc=-icompiler/stage2/build --optghc=-icompiler/stage2/build/autogen --optghc=-Icompiler/stage2/build --optghc=-Icompiler/stage2/build/autogen --optghc=-Icompiler/. --optghc=-Icompiler/parser --optghc=-Icompiler/utils --optghc=-Icompiler/../rts/dist/build --optghc=-Icompiler/stage2 --optghc=-optP-DGHCI --optghc=-optP-include --optghc=-optPcompiler/stage2/build/autogen/cabal_macros.h --optghc=-package --optghc=Cabal-1.18.0 --optghc=-package --optghc=array-0.4.0.2 --optghc=-package --optghc=base-4.7.0.0 --optghc=-package --optghc=bin-package-db-0.0.0.0 --optghc=-package --optghc=bytestring-0.10.3.0 --optghc=-package --optghc=containers-0.5.0.0 --optghc=-package --optghc=directory-1.2.0.1 --optghc=-package --optghc=filepath-1.3.0.2 --optghc=-package --optghc=hoopl-3.10.0.0 --optghc=-package --optghc=hpc-0.6.0.1 --optghc=-package --optghc=process-1.2.0.0 --optghc=-package --optghc=template-haskell-2.9.0.0 --optghc=-package --optghc=time-1.4.0.2 --optghc=-package --optghc=transformers-0.3.0.0 --optghc=-package --optghc=unix-2.7.0.0 --optghc=-Wall --optghc=-fno-warn-name-shadowing --optghc=-XHaskell98 --optghc=-XCPP --optghc=-XMagicHash --optghc=-XUnboxedTuples --optghc=-XPatternGuards --optghc=-XForeignFunctionInterface --optghc=-XEmptyDataDecls --optghc=-XTypeSynonymInstances --optghc=-XMultiParamTypeClasses --optghc=-XFlexibleInstances --optghc=-XRankNTypes --optghc=-XScopedTypeVariables --optghc=-XDeriveDataTypeable --optghc=-XBangPatterns --optghc=-XNondecreasingIndentation --optghc=-DGHCI_TABLES_NEXT_TO_CODE --optghc=-DSTAGE=2 --optghc=-O0 --optghc=-fasm --optghc=-no-user-package-db --optghc=-rtsopts --optghc=-odir --optghc=compiler/stage2/build --optghc=-hidir --optghc=compiler/stage2/build --optghc=-stubdir --optghc=compiler/stage2/build --source-module=src/%{MODULE/./-}.html --source-entity=src/%{MODULE/./-}.html#%{NAME} --optghc=-DSTAGE=2 compiler/basicTypes/Avail.hs compiler/basicTypes/BasicTypes.lhs compiler/basicTypes/DataCon.lhs compiler/basicTypes/Demand.lhs compiler/utils/Exception.hs compiler/main/GhcMonad.hs compiler/basicTypes/Id.lhs compiler/basicTypes/IdInfo.lhs compiler/basicTypes/Literal.lhs compiler/llvmGen/Llvm.hs compiler/llvmGen/Llvm/AbsSyn.hs compiler/llvmGen/Llvm/MetaData.hs compiler/llvmGen/Llvm/PpLlvm.hs compiler/llvmGen/Llvm/Types.hs compiler/llvmGen/LlvmCodeGen.hs compiler/llvmGen/LlvmCodeGen/Base.hs compiler/llvmGen/LlvmCodeGen/CodeGen.hs compiler/llvmGen/LlvmCodeGen/Data.hs compiler/llvmGen/LlvmCodeGen/Ppr.hs compiler/llvmGen/LlvmCodeGen/Regs.hs compiler/llvmGen/LlvmMangler.hs compiler/basicTypes/MkId.lhs compiler/basicTypes/Module.lhs compiler/basicTypes/Name.lhs compiler/basicTypes/NameEnv.lhs compiler/basicTypes/NameSet.lhs compiler/basicTypes/OccName.lhs compiler/basicTypes/RdrName.lhs compiler/basicTypes/SrcLoc.lhs compiler/basicTypes/UniqSupply.lhs compiler/basicTypes/Unique.lhs compiler/basicTypes/Var.lhs compiler/basicTypes/VarEnv.lhs compiler/basicTypes/VarSet.lhs compiler/cmm/BlockId.hs compiler/cmm/CLabel.hs compiler/cmm/Cmm.hs compiler/cmm/CmmBuildInfoTables.hs compiler/cmm/CmmPipeline.hs compiler/cmm/CmmCallConv.hs compiler/cmm/CmmCommonBlockElim.hs compiler/cmm/CmmContFlowOpt.hs compiler/cmm/CmmExpr.hs compiler/cmm/CmmInfo.hs compiler/stage2/build/CmmLex.hs compiler/cmm/CmmLint.hs compiler/cmm/CmmLive.hs compiler/cmm/CmmMachOp.hs compiler/cmm/CmmNode.hs compiler/cmm/CmmOpt.hs compiler/stage2/build/CmmParse.hs compiler/cmm/CmmProcPoint.hs compiler/cmm/CmmRewriteAssignments.hs compiler/cmm/CmmSink.hs compiler/cmm/CmmType.hs compiler/cmm/CmmUtils.hs compiler/cmm/CmmLayoutStack.hs compiler/cmm/MkGraph.hs compiler/nativeGen/PprBase.hs compiler/cmm/PprC.hs compiler/cmm/PprCmm.hs compiler/cmm/PprCmmDecl.hs compiler/cmm/PprCmmExpr.hs compiler/cmm/Bitmap.hs compiler/codeGen/CodeGen/Platform.hs compiler/codeGen/CodeGen/Platform/ARM.hs compiler/codeGen/CodeGen/Platform/NoRegs.hs compiler/codeGen/CodeGen/Platform/PPC.hs compiler/codeGen/CodeGen/Platform/PPC_Darwin.hs compiler/codeGen/CodeGen/Platform/SPARC.hs compiler/codeGen/CodeGen/Platform/X86.hs compiler/codeGen/CodeGen/Platform/X86_64.hs compiler/codeGen/CgUtils.hs compiler/codeGen/StgCmm.hs compiler/codeGen/StgCmmBind.hs compiler/codeGen/StgCmmClosure.hs compiler/codeGen/StgCmmCon.hs compiler/codeGen/StgCmmEnv.hs compiler/codeGen/StgCmmExpr.hs compiler/codeGen/StgCmmForeign.hs compiler/codeGen/StgCmmHeap.hs compiler/codeGen/StgCmmHpc.hs compiler/codeGen/StgCmmArgRep.hs compiler/codeGen/StgCmmLayout.hs compiler/codeGen/StgCmmMonad.hs compiler/codeGen/StgCmmPrim.hs compiler/codeGen/StgCmmProf.hs compiler/codeGen/StgCmmTicky.hs compiler/codeGen/StgCmmUtils.hs compiler/codeGen/StgCmmExtCode.hs compiler/cmm/SMRep.lhs compiler/coreSyn/CoreArity.lhs compiler/coreSyn/CoreFVs.lhs compiler/coreSyn/CoreLint.lhs compiler/coreSyn/CorePrep.lhs compiler/coreSyn/CoreSubst.lhs compiler/coreSyn/CoreSyn.lhs compiler/coreSyn/TrieMap.lhs compiler/coreSyn/CoreTidy.lhs compiler/coreSyn/CoreUnfold.lhs compiler/coreSyn/CoreUtils.lhs compiler/coreSyn/ExternalCore.lhs compiler/coreSyn/MkCore.lhs compiler/coreSyn/MkExternalCore.lhs compiler/coreSyn/PprCore.lhs compiler/coreSyn/PprExternalCore.lhs compiler/deSugar/Check.lhs compiler/deSugar/Coverage.lhs compiler/deSugar/Desugar.lhs compiler/deSugar/DsArrows.lhs compiler/deSugar/DsBinds.lhs compiler/deSugar/DsCCall.lhs compiler/deSugar/DsExpr.lhs compiler/deSugar/DsForeign.lhs compiler/deSugar/DsGRHSs.lhs compiler/deSugar/DsListComp.lhs compiler/deSugar/DsMonad.lhs compiler/deSugar/DsUtils.lhs compiler/deSugar/Match.lhs compiler/deSugar/MatchCon.lhs compiler/deSugar/MatchLit.lhs compiler/hsSyn/HsBinds.lhs compiler/hsSyn/HsDecls.lhs compiler/hsSyn/HsDoc.hs compiler/hsSyn/HsExpr.lhs compiler/hsSyn/HsImpExp.lhs compiler/hsSyn/HsLit.lhs compiler/hsSyn/HsPat.lhs compiler/hsSyn/HsSyn.lhs compiler/hsSyn/HsTypes.lhs compiler/hsSyn/HsUtils.lhs compiler/iface/BinIface.hs compiler/iface/BuildTyCl.lhs compiler/iface/IfaceEnv.lhs compiler/iface/IfaceSyn.lhs compiler/iface/IfaceType.lhs compiler/iface/LoadIface.lhs compiler/iface/MkIface.lhs compiler/iface/TcIface.lhs compiler/iface/FlagChecker.hs compiler/main/Annotations.hs compiler/main/BreakArray.hs compiler/main/CmdLineParser.hs compiler/main/CodeOutput.lhs compiler/stage2/build/Config.hs compiler/main/Constants.lhs compiler/main/DriverMkDepend.hs compiler/main/DriverPhases.hs compiler/main/DriverPipeline.hs compiler/main/DynFlags.hs compiler/main/ErrUtils.lhs compiler/main/Finder.lhs compiler/main/GHC.hs compiler/main/GhcMake.hs compiler/main/GhcPlugins.hs compiler/main/DynamicLoading.hs compiler/main/HeaderInfo.hs compiler/main/HscMain.hs compiler/main/HscStats.hs compiler/main/HscTypes.lhs compiler/main/InteractiveEval.hs compiler/main/InteractiveEvalTypes.hs compiler/main/PackageConfig.hs compiler/main/Packages.lhs compiler/main/PlatformConstants.hs compiler/main/PprTyThing.hs compiler/main/StaticFlags.hs compiler/main/SysTools.lhs compiler/main/TidyPgm.lhs compiler/parser/Ctype.lhs compiler/parser/HaddockUtils.hs compiler/parser/LexCore.hs compiler/stage2/build/Lexer.hs compiler/types/OptCoercion.lhs compiler/stage2/build/Parser.hs compiler/stage2/build/ParserCore.hs compiler/parser/ParserCoreUtils.hs compiler/parser/RdrHsSyn.lhs compiler/prelude/ForeignCall.lhs compiler/prelude/PrelInfo.lhs compiler/prelude/PrelNames.lhs compiler/prelude/PrelRules.lhs compiler/prelude/PrimOp.lhs compiler/prelude/TysPrim.lhs compiler/prelude/TysWiredIn.lhs compiler/profiling/CostCentre.lhs compiler/profiling/ProfInit.hs compiler/profiling/SCCfinal.lhs compiler/rename/RnBinds.lhs compiler/rename/RnEnv.lhs compiler/rename/RnExpr.lhs compiler/rename/RnHsDoc.hs compiler/rename/RnNames.lhs compiler/rename/RnPat.lhs compiler/rename/RnSource.lhs compiler/rename/RnTypes.lhs compiler/simplCore/CoreMonad.lhs compiler/simplCore/CSE.lhs compiler/simplCore/FloatIn.lhs compiler/simplCore/FloatOut.lhs compiler/simplCore/LiberateCase.lhs compiler/simplCore/OccurAnal.lhs compiler/simplCore/SAT.lhs compiler/simplCore/SetLevels.lhs compiler/simplCore/SimplCore.lhs compiler/simplCore/SimplEnv.lhs compiler/simplCore/SimplMonad.lhs compiler/simplCore/SimplUtils.lhs compiler/simplCore/Simplify.lhs compiler/simplStg/SimplStg.lhs compiler/simplStg/StgStats.lhs compiler/simplStg/UnariseStg.lhs compiler/specialise/Rules.lhs compiler/specialise/SpecConstr.lhs compiler/specialise/Specialise.lhs compiler/stgSyn/CoreToStg.lhs compiler/stgSyn/StgLint.lhs compiler/stgSyn/StgSyn.lhs compiler/stranal/DmdAnal.lhs compiler/stranal/WorkWrap.lhs compiler/stranal/WwLib.lhs compiler/typecheck/FamInst.lhs compiler/typecheck/Inst.lhs compiler/typecheck/TcAnnotations.lhs compiler/typecheck/TcArrows.lhs compiler/typecheck/TcBinds.lhs compiler/typecheck/TcClassDcl.lhs compiler/typecheck/TcDefaults.lhs compiler/typecheck/TcDeriv.lhs compiler/typecheck/TcEnv.lhs compiler/typecheck/TcExpr.lhs compiler/typecheck/TcForeign.lhs compiler/typecheck/TcGenDeriv.lhs compiler/typecheck/TcGenGenerics.lhs compiler/typecheck/TcHsSyn.lhs compiler/typecheck/TcHsType.lhs compiler/typecheck/TcInstDcls.lhs compiler/typecheck/TcMType.lhs compiler/typecheck/TcValidity.lhs compiler/typecheck/TcMatches.lhs compiler/typecheck/TcPat.lhs compiler/typecheck/TcRnDriver.lhs compiler/typecheck/TcRnMonad.lhs compiler/typecheck/TcRnTypes.lhs compiler/typecheck/TcRules.lhs compiler/typecheck/TcSimplify.lhs compiler/typecheck/TcErrors.lhs compiler/typecheck/TcTyClsDecls.lhs compiler/typecheck/TcTyDecls.lhs compiler/typecheck/TcType.lhs compiler/typecheck/TcEvidence.lhs compiler/typecheck/TcUnify.lhs compiler/typecheck/TcInteract.lhs compiler/typecheck/TcCanonical.lhs compiler/typecheck/TcSMonad.lhs compiler/types/Class.lhs compiler/types/Coercion.lhs compiler/types/FamInstEnv.lhs compiler/types/FunDeps.lhs compiler/types/InstEnv.lhs compiler/types/TyCon.lhs compiler/types/CoAxiom.lhs compiler/types/Kind.lhs compiler/types/Type.lhs compiler/types/TypeRep.lhs compiler/types/Unify.lhs compiler/utils/Bag.lhs compiler/utils/Binary.hs compiler/utils/BufWrite.hs compiler/utils/Digraph.lhs compiler/utils/Encoding.hs compiler/utils/FastBool.lhs compiler/utils/FastFunctions.lhs compiler/utils/FastMutInt.lhs compiler/utils/FastString.lhs compiler/utils/FastTypes.lhs compiler/stage2/build/Fingerprint.hs compiler/utils/FiniteMap.lhs compiler/utils/GraphBase.hs compiler/utils/GraphColor.hs compiler/utils/GraphOps.hs compiler/utils/GraphPpr.hs compiler/utils/IOEnv.hs compiler/utils/ListSetOps.lhs compiler/utils/Maybes.lhs compiler/utils/MonadUtils.hs compiler/utils/OrdList.lhs compiler/utils/Outputable.lhs compiler/utils/Pair.lhs compiler/utils/Panic.lhs compiler/utils/Pretty.lhs compiler/utils/Serialized.hs compiler/utils/State.hs compiler/utils/Stream.hs compiler/utils/StringBuffer.lhs compiler/utils/UniqFM.lhs compiler/utils/UniqSet.lhs compiler/utils/Util.lhs compiler/vectorise/Vectorise/Builtins/Base.hs compiler/vectorise/Vectorise/Builtins/Initialise.hs compiler/vectorise/Vectorise/Builtins.hs compiler/vectorise/Vectorise/Monad/Base.hs compiler/vectorise/Vectorise/Monad/Naming.hs compiler/vectorise/Vectorise/Monad/Local.hs compiler/vectorise/Vectorise/Monad/Global.hs compiler/vectorise/Vectorise/Monad/InstEnv.hs compiler/vectorise/Vectorise/Monad.hs compiler/vectorise/Vectorise/Utils/Base.hs compiler/vectorise/Vectorise/Utils/Closure.hs compiler/vectorise/Vectorise/Utils/Hoisting.hs compiler/vectorise/Vectorise/Utils/PADict.hs compiler/vectorise/Vectorise/Utils/Poly.hs compiler/vectorise/Vectorise/Utils.hs compiler/vectorise/Vectorise/Generic/Description.hs compiler/vectorise/Vectorise/Generic/PAMethods.hs compiler/vectorise/Vectorise/Generic/PADict.hs compiler/vectorise/Vectorise/Generic/PData.hs compiler/vectorise/Vectorise/Type/Env.hs compiler/vectorise/Vectorise/Type/Type.hs compiler/vectorise/Vectorise/Type/TyConDecl.hs compiler/vectorise/Vectorise/Type/Classify.hs compiler/vectorise/Vectorise/Convert.hs compiler/vectorise/Vectorise/Vect.hs compiler/vectorise/Vectorise/Var.hs compiler/vectorise/Vectorise/Env.hs compiler/vectorise/Vectorise/Exp.hs compiler/vectorise/Vectorise.hs compiler/cmm/Hoopl/Dataflow.hs compiler/cmm/Hoopl.hs compiler/nativeGen/AsmCodeGen.lhs compiler/nativeGen/TargetReg.hs compiler/nativeGen/NCGMonad.hs compiler/nativeGen/Instruction.hs compiler/nativeGen/Size.hs compiler/nativeGen/Reg.hs compiler/nativeGen/RegClass.hs compiler/nativeGen/PIC.hs compiler/utils/Platform.hs compiler/nativeGen/CPrim.hs compiler/nativeGen/X86/Regs.hs compiler/nativeGen/X86/RegInfo.hs compiler/nativeGen/X86/Instr.hs compiler/nativeGen/X86/Cond.hs compiler/nativeGen/X86/Ppr.hs compiler/nativeGen/X86/CodeGen.hs compiler/nativeGen/PPC/Regs.hs compiler/nativeGen/PPC/RegInfo.hs compiler/nativeGen/PPC/Instr.hs compiler/nativeGen/PPC/Cond.hs compiler/nativeGen/PPC/Ppr.hs compiler/nativeGen/PPC/CodeGen.hs compiler/nativeGen/SPARC/Base.hs compiler/nativeGen/SPARC/Regs.hs compiler/nativeGen/SPARC/Imm.hs compiler/nativeGen/SPARC/AddrMode.hs compiler/nativeGen/SPARC/Cond.hs compiler/nativeGen/SPARC/Instr.hs compiler/nativeGen/SPARC/Stack.hs compiler/nativeGen/SPARC/ShortcutJump.hs compiler/nativeGen/SPARC/Ppr.hs compiler/nativeGen/SPARC/CodeGen.hs compiler/nativeGen/SPARC/CodeGen/Amode.hs compiler/nativeGen/SPARC/CodeGen/Base.hs compiler/nativeGen/SPARC/CodeGen/CondCode.hs compiler/nativeGen/SPARC/CodeGen/Gen32.hs compiler/nativeGen/SPARC/CodeGen/Gen64.hs compiler/nativeGen/SPARC/CodeGen/Sanity.hs compiler/nativeGen/SPARC/CodeGen/Expand.hs compiler/nativeGen/RegAlloc/Liveness.hs compiler/nativeGen/RegAlloc/Graph/Main.hs compiler/nativeGen/RegAlloc/Graph/Stats.hs compiler/nativeGen/RegAlloc/Graph/ArchBase.hs compiler/nativeGen/RegAlloc/Graph/ArchX86.hs compiler/nativeGen/RegAlloc/Graph/Coalesce.hs compiler/nativeGen/RegAlloc/Graph/Spill.hs compiler/nativeGen/RegAlloc/Graph/SpillClean.hs compiler/nativeGen/RegAlloc/Graph/SpillCost.hs compiler/nativeGen/RegAlloc/Graph/TrivColorable.hs compiler/nativeGen/RegAlloc/Linear/Main.hs compiler/nativeGen/RegAlloc/Linear/JoinToTargets.hs compiler/nativeGen/RegAlloc/Linear/State.hs compiler/nativeGen/RegAlloc/Linear/Stats.hs compiler/nativeGen/RegAlloc/Linear/FreeRegs.hs compiler/nativeGen/RegAlloc/Linear/StackMap.hs compiler/nativeGen/RegAlloc/Linear/Base.hs compiler/nativeGen/RegAlloc/Linear/X86/FreeRegs.hs compiler/nativeGen/RegAlloc/Linear/X86_64/FreeRegs.hs compiler/nativeGen/RegAlloc/Linear/PPC/FreeRegs.hs compiler/nativeGen/RegAlloc/Linear/SPARC/FreeRegs.hs compiler/deSugar/DsMeta.hs compiler/typecheck/TcSplice.lhs compiler/hsSyn/Convert.lhs compiler/ghci/ByteCodeAsm.lhs compiler/ghci/ByteCodeGen.lhs compiler/ghci/ByteCodeInstr.lhs compiler/ghci/ByteCodeItbls.lhs compiler/ghci/ByteCodeLink.lhs compiler/ghci/Debugger.hs compiler/stage2/build/LibFFI.hs compiler/ghci/Linker.lhs compiler/ghci/ObjLink.lhs compiler/ghci/RtClosureInspect.hs compiler/ghci/DebuggerUtils.hs +RTS -tcompiler/stage2/doc/html/ghc/ghc.haddock.t --machine-readable 
 [("bytes allocated", "2125377156")
 ,("num_GCs", "238655")
 ,("average_bytes_used", "117568519")
 ,("max_bytes_used", "244802940")
 ,("num_byte_usage_samples", "27")
 ,("peak_megabytes_allocated", "690")
 ,("init_cpu_seconds", "0.00")
 ,("init_wall_seconds", "0.00")
 ,("mutator_cpu_seconds", "240.95")
 ,("mutator_wall_seconds", "268.30")
 ,("GC_cpu_seconds", "69.42")
 ,("GC_wall_seconds", "47.41")
 ]
