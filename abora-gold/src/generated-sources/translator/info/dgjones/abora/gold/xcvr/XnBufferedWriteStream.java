/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.xcvr;

import info.dgjones.abora.gold.appmods.WorksIniter;
import info.dgjones.abora.gold.arrange.Arrangement;
import info.dgjones.abora.gold.aspire.PtrArrayAccumulator;
import info.dgjones.abora.gold.backend.DiskManagerEmulsion;
import info.dgjones.abora.gold.backrec.DirectEditionRecorder;
import info.dgjones.abora.gold.backrec.DirectWorkRecorder;
import info.dgjones.abora.gold.backrec.EditionRecorder;
import info.dgjones.abora.gold.backrec.IndirectEditionRecorder;
import info.dgjones.abora.gold.backrec.IndirectWorkRecorder;
import info.dgjones.abora.gold.backrec.ResultRecorder;
import info.dgjones.abora.gold.backrec.WorkRecorder;
import info.dgjones.abora.gold.be.basic.BeCarrier;
import info.dgjones.abora.gold.be.basic.BeClub;
import info.dgjones.abora.gold.be.basic.BeDataHolder;
import info.dgjones.abora.gold.be.basic.BeEdition;
import info.dgjones.abora.gold.be.basic.BeGrandMap;
import info.dgjones.abora.gold.be.basic.BeIDHolder;
import info.dgjones.abora.gold.be.basic.BeLabel;
import info.dgjones.abora.gold.be.basic.BePlaceHolder;
import info.dgjones.abora.gold.be.basic.BeRangeElement;
import info.dgjones.abora.gold.be.basic.BeWork;
import info.dgjones.abora.gold.be.basic.ID;
import info.dgjones.abora.gold.be.canopy.AbstractRecorderFinder;
import info.dgjones.abora.gold.be.canopy.AnyRecorderEFinder;
import info.dgjones.abora.gold.be.canopy.AnyRecorderFinder;
import info.dgjones.abora.gold.be.canopy.AnyRecorderPFinder;
import info.dgjones.abora.gold.be.canopy.BackfollowFinder;
import info.dgjones.abora.gold.be.canopy.BackfollowPFinder;
import info.dgjones.abora.gold.be.canopy.BertCrum;
import info.dgjones.abora.gold.be.canopy.BertPropFinder;
import info.dgjones.abora.gold.be.canopy.CannotPartializeFinder;
import info.dgjones.abora.gold.be.canopy.CanopyCache;
import info.dgjones.abora.gold.be.canopy.CanopyCrum;
import info.dgjones.abora.gold.be.canopy.ClosedPropFinder;
import info.dgjones.abora.gold.be.canopy.ContainedEditionRecorderEFinder;
import info.dgjones.abora.gold.be.canopy.CumulativeRecorderFinder;
import info.dgjones.abora.gold.be.canopy.OpenPropFinder;
import info.dgjones.abora.gold.be.canopy.OriginalResultRecorderEFinder;
import info.dgjones.abora.gold.be.canopy.PartialityFinder;
import info.dgjones.abora.gold.be.canopy.PropFinder;
import info.dgjones.abora.gold.be.canopy.ResultRecorderPFinder;
import info.dgjones.abora.gold.be.canopy.SensorCrum;
import info.dgjones.abora.gold.be.canopy.SensorFinder;
import info.dgjones.abora.gold.be.canopy.SensorPropFinder;
import info.dgjones.abora.gold.be.canopy.SimpleRecorderFinder;
import info.dgjones.abora.gold.be.canopy.prop.BertProp;
import info.dgjones.abora.gold.be.canopy.prop.Prop;
import info.dgjones.abora.gold.be.canopy.prop.SensorProp;
import info.dgjones.abora.gold.be.ents.ActualOrglRoot;
import info.dgjones.abora.gold.be.ents.DspLoaf;
import info.dgjones.abora.gold.be.ents.EmptyOrglRoot;
import info.dgjones.abora.gold.be.ents.Ent;
import info.dgjones.abora.gold.be.ents.HBottomCrum;
import info.dgjones.abora.gold.be.ents.HUpperCrum;
import info.dgjones.abora.gold.be.ents.HistoryCrum;
import info.dgjones.abora.gold.be.ents.InnerLoaf;
import info.dgjones.abora.gold.be.ents.Loaf;
import info.dgjones.abora.gold.be.ents.MergeBundlesStepper;
import info.dgjones.abora.gold.be.ents.OExpandingLoaf;
import info.dgjones.abora.gold.be.ents.OPart;
import info.dgjones.abora.gold.be.ents.OPartialLoaf;
import info.dgjones.abora.gold.be.ents.OVirtualLoaf;
import info.dgjones.abora.gold.be.ents.OrglRoot;
import info.dgjones.abora.gold.be.ents.RegionLoaf;
import info.dgjones.abora.gold.be.ents.SharedData;
import info.dgjones.abora.gold.be.ents.SplitLoaf;
import info.dgjones.abora.gold.be.locks.BooLock;
import info.dgjones.abora.gold.be.locks.ChallengeLock;
import info.dgjones.abora.gold.be.locks.Lock;
import info.dgjones.abora.gold.be.locks.MatchLock;
import info.dgjones.abora.gold.be.locks.MultiLock;
import info.dgjones.abora.gold.be.locks.WallLock;
import info.dgjones.abora.gold.brange1.FillDetectorExecutor;
import info.dgjones.abora.gold.brange2.BeWorkLockExecutor;
import info.dgjones.abora.gold.brange2.RevisionWatcherExecutor;
import info.dgjones.abora.gold.brange2.UpdateTransitiveMemberIDs;
import info.dgjones.abora.gold.brange2.UpdateTransitiveSuperClubIDs;
import info.dgjones.abora.gold.brange3.BeEditionDetectorExecutor;
import info.dgjones.abora.gold.cache.CacheManager;
import info.dgjones.abora.gold.cache.InstanceCache;
import info.dgjones.abora.gold.cache.SuspendedHeaper;
import info.dgjones.abora.gold.calc.PrintCBlocksTracks;
import info.dgjones.abora.gold.calc.TrackCBlocks;
import info.dgjones.abora.gold.canopy.Heaper2UInt32Cache;
import info.dgjones.abora.gold.chameleon.Butterfly;
import info.dgjones.abora.gold.chameleon.Chameleon;
import info.dgjones.abora.gold.chameleon.DeadMoth;
import info.dgjones.abora.gold.chameleon.GoldButterfly;
import info.dgjones.abora.gold.chameleon.IronButterfly;
import info.dgjones.abora.gold.chameleon.LeadButterfly;
import info.dgjones.abora.gold.chameleon.Moth;
import info.dgjones.abora.gold.cobbler.ActualCookbook;
import info.dgjones.abora.gold.cobbler.BootMaker;
import info.dgjones.abora.gold.cobbler.BootPlan;
import info.dgjones.abora.gold.cobbler.ClearPlan;
import info.dgjones.abora.gold.cobbler.Connection;
import info.dgjones.abora.gold.cobbler.Cookbook;
import info.dgjones.abora.gold.cobbler.DirectConnection;
import info.dgjones.abora.gold.cobbler.DiskConnection;
import info.dgjones.abora.gold.cobbler.FromDiskPlan;
import info.dgjones.abora.gold.cobbler.NestedConnection;
import info.dgjones.abora.gold.collection.basic.UInt8Array;
import info.dgjones.abora.gold.collection.cache.HashSetCache;
import info.dgjones.abora.gold.collection.grand.ExponentialHashMap;
import info.dgjones.abora.gold.collection.grand.GrandDataPage;
import info.dgjones.abora.gold.collection.grand.GrandEntry;
import info.dgjones.abora.gold.collection.grand.GrandHashSet;
import info.dgjones.abora.gold.collection.grand.GrandHashSetTester;
import info.dgjones.abora.gold.collection.grand.GrandHashTable;
import info.dgjones.abora.gold.collection.grand.GrandHashTableTester;
import info.dgjones.abora.gold.collection.grand.GrandNode;
import info.dgjones.abora.gold.collection.grand.GrandOverflow;
import info.dgjones.abora.gold.collection.grand.GrandSetEntry;
import info.dgjones.abora.gold.collection.grand.GrandTableEntry;
import info.dgjones.abora.gold.collection.sets.ActualHashSet;
import info.dgjones.abora.gold.collection.sets.EmptyImmuSet;
import info.dgjones.abora.gold.collection.sets.HashSet;
import info.dgjones.abora.gold.collection.sets.ImmuSet;
import info.dgjones.abora.gold.collection.sets.ImmuSetOnMu;
import info.dgjones.abora.gold.collection.sets.MuSet;
import info.dgjones.abora.gold.collection.sets.ScruSet;
import info.dgjones.abora.gold.collection.sets.SetAccumulator;
import info.dgjones.abora.gold.collection.sets.TinyImmuSet;
import info.dgjones.abora.gold.collection.sets.UnionRecruiter;
import info.dgjones.abora.gold.collection.settable.BucketArrayStepper;
import info.dgjones.abora.gold.collection.settable.HashIndexEntry;
import info.dgjones.abora.gold.collection.settable.HeaperAsEntry;
import info.dgjones.abora.gold.collection.settable.IndexEntry;
import info.dgjones.abora.gold.collection.settable.PositionEntry;
import info.dgjones.abora.gold.collection.settable.SetTable;
import info.dgjones.abora.gold.collection.settable.SetTableTester;
import info.dgjones.abora.gold.collection.settable.TableEntry;
import info.dgjones.abora.gold.collection.steppers.Accumulator;
import info.dgjones.abora.gold.collection.steppers.ArrayAccumulator;
import info.dgjones.abora.gold.collection.steppers.ArrayStepper;
import info.dgjones.abora.gold.collection.steppers.AscendingArrayStepper;
import info.dgjones.abora.gold.collection.steppers.DisjointRegionStepper;
import info.dgjones.abora.gold.collection.steppers.HashSetStepper;
import info.dgjones.abora.gold.collection.steppers.ITAscendingStepper;
import info.dgjones.abora.gold.collection.steppers.ITDescendingStepper;
import info.dgjones.abora.gold.collection.steppers.ITGenericStepper;
import info.dgjones.abora.gold.collection.steppers.IntegerTableStepper;
import info.dgjones.abora.gold.collection.steppers.OffsetArrayStepper;
import info.dgjones.abora.gold.collection.steppers.OffsetScruTableStepper;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.collection.steppers.TableAccumulator;
import info.dgjones.abora.gold.collection.steppers.TableStepper;
import info.dgjones.abora.gold.collection.tables.ActualArray;
import info.dgjones.abora.gold.collection.tables.ActualHashTable;
import info.dgjones.abora.gold.collection.tables.ActualIntegerTable;
import info.dgjones.abora.gold.collection.tables.COWIntegerTable;
import info.dgjones.abora.gold.collection.tables.HashTable;
import info.dgjones.abora.gold.collection.tables.ImmuTable;
import info.dgjones.abora.gold.collection.tables.ImmuTableOnMu;
import info.dgjones.abora.gold.collection.tables.IntegerScruTable;
import info.dgjones.abora.gold.collection.tables.IntegerTable;
import info.dgjones.abora.gold.collection.tables.MuArray;
import info.dgjones.abora.gold.collection.tables.MuTable;
import info.dgjones.abora.gold.collection.tables.OberIntegerTable;
import info.dgjones.abora.gold.collection.tables.OffsetImmuTable;
import info.dgjones.abora.gold.collection.tables.OffsetScruArray;
import info.dgjones.abora.gold.collection.tables.OffsetScruTable;
import info.dgjones.abora.gold.collection.tables.Pair;
import info.dgjones.abora.gold.collection.tables.ScruTable;
import info.dgjones.abora.gold.counter.BatchCounter;
import info.dgjones.abora.gold.counter.Counter;
import info.dgjones.abora.gold.counter.MultiCounter;
import info.dgjones.abora.gold.counter.SingleCounter;
import info.dgjones.abora.gold.cross.BoxAccumulator;
import info.dgjones.abora.gold.cross.CrossMapping;
import info.dgjones.abora.gold.cross.CrossOrderSpec;
import info.dgjones.abora.gold.cross.CrossTester;
import info.dgjones.abora.gold.cross.GenericCrossDsp;
import info.dgjones.abora.gold.cross.GenericCrossSimpleRegionStepper;
import info.dgjones.abora.gold.cross.TupleStepper;
import info.dgjones.abora.gold.cxx.classx.comm.CategoryRecipe;
import info.dgjones.abora.gold.cxx.classx.comm.StubRecipe;
import info.dgjones.abora.gold.cxx.classx.other.CommentThunk;
import info.dgjones.abora.gold.cxx.classx.other.EchoThunk;
import info.dgjones.abora.gold.cxx.classx.stuff.DeadButterfly;
import info.dgjones.abora.gold.cxx.classx.stuff.GrandDataPageStepper;
import info.dgjones.abora.gold.cxx.classx.stuff.GrandHashSetStepper;
import info.dgjones.abora.gold.cxx.classx.stuff.GrandHashTableStepper;
import info.dgjones.abora.gold.cxx.classx.stuff.GrandNodeStepper;
import info.dgjones.abora.gold.cxx.classx.stuff.GrandOverflowStepper;
import info.dgjones.abora.gold.cxx.otherclass.CopyRecipe;
import info.dgjones.abora.gold.detect.FeDetector;
import info.dgjones.abora.gold.detect.FeFillDetector;
import info.dgjones.abora.gold.detect.FeFillRangeDetector;
import info.dgjones.abora.gold.detect.FeRevisionDetector;
import info.dgjones.abora.gold.detect.FeStatusDetector;
import info.dgjones.abora.gold.detect.FeWaitDetector;
import info.dgjones.abora.gold.diskman.Cattleman;
import info.dgjones.abora.gold.diskman.DiskTester;
import info.dgjones.abora.gold.diskman.HonestAbeIniter;
import info.dgjones.abora.gold.diskman.Honestly;
import info.dgjones.abora.gold.edge.EdgeManager;
import info.dgjones.abora.gold.edgeregion.EdgeAccumulator;
import info.dgjones.abora.gold.edgeregion.EdgeSimpleRegionStepper;
import info.dgjones.abora.gold.edgeregion.EdgeStepper;
import info.dgjones.abora.gold.edgeregion.TransitionEdge;
import info.dgjones.abora.gold.fbtest.BackendBootMaker;
import info.dgjones.abora.gold.fbtest.FeWorksBootMaker;
import info.dgjones.abora.gold.fbtest.ShepherdBootMaker;
import info.dgjones.abora.gold.fbtest.WorksBootMaker;
import info.dgjones.abora.gold.filter.AndFilter;
import info.dgjones.abora.gold.filter.ClosedFilter;
import info.dgjones.abora.gold.filter.Filter;
import info.dgjones.abora.gold.filter.FilterDsp;
import info.dgjones.abora.gold.filter.FilterPosition;
import info.dgjones.abora.gold.filter.FilterSpace;
import info.dgjones.abora.gold.filter.Joint;
import info.dgjones.abora.gold.filter.NotSubsetFilter;
import info.dgjones.abora.gold.filter.NotSupersetFilter;
import info.dgjones.abora.gold.filter.OpenFilter;
import info.dgjones.abora.gold.filter.OrFilter;
import info.dgjones.abora.gold.filter.RealDsp;
import info.dgjones.abora.gold.filter.RegionDelta;
import info.dgjones.abora.gold.filter.SubsetFilter;
import info.dgjones.abora.gold.filter.SupersetFilter;
import info.dgjones.abora.gold.fm.support.Thunk;
import info.dgjones.abora.gold.fossil.DirectEditionRecorderFossil;
import info.dgjones.abora.gold.fossil.DirectWorkRecorderFossil;
import info.dgjones.abora.gold.fossil.EditionRecorderFossil;
import info.dgjones.abora.gold.fossil.IndirectEditionRecorderFossil;
import info.dgjones.abora.gold.fossil.IndirectWorkRecorderFossil;
import info.dgjones.abora.gold.fossil.RecorderFossil;
import info.dgjones.abora.gold.fossil.WorkRecorderFossil;
import info.dgjones.abora.gold.gchooks.CloseExecutor;
import info.dgjones.abora.gold.gchooks.DeleteExecutor;
import info.dgjones.abora.gold.gchooks.RepairEngineer;
import info.dgjones.abora.gold.gchooks.SanitationEngineer;
import info.dgjones.abora.gold.grantab.GrandNodeDoubler;
import info.dgjones.abora.gold.grantab.GrandNodeReinserter;
import info.dgjones.abora.gold.hlogger.LogTester;
import info.dgjones.abora.gold.hlogger.SwitchLogger;
import info.dgjones.abora.gold.hspace.HeaperRegion;
import info.dgjones.abora.gold.id.IDRegion;
import info.dgjones.abora.gold.id.IDSimpleStepper;
import info.dgjones.abora.gold.id.IDStepper;
import info.dgjones.abora.gold.id.RealStepper;
import info.dgjones.abora.gold.id.SequenceStepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.CxxSystemOrganization;
import info.dgjones.abora.gold.java.missing.CxxTreeAssociation;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.lock.Encrypter;
import info.dgjones.abora.gold.lock.EncrypterMaker;
import info.dgjones.abora.gold.lock.NoEncrypter;
import info.dgjones.abora.gold.lock.NoScrambler;
import info.dgjones.abora.gold.lock.Scrambler;
import info.dgjones.abora.gold.nadmin.DefaultSession;
import info.dgjones.abora.gold.nadmin.FeBooLockSmith;
import info.dgjones.abora.gold.nadmin.FeChallengeLockSmith;
import info.dgjones.abora.gold.nadmin.FeClubDescription;
import info.dgjones.abora.gold.nadmin.FeLockSmith;
import info.dgjones.abora.gold.nadmin.FeMatchLockSmith;
import info.dgjones.abora.gold.nadmin.FeMultiLockSmith;
import info.dgjones.abora.gold.nadmin.FePromiseSession;
import info.dgjones.abora.gold.nadmin.FeSession;
import info.dgjones.abora.gold.nadmin.FeWallLockSmith;
import info.dgjones.abora.gold.nbacken.EditionStepper;
import info.dgjones.abora.gold.nbacken.GrantStepper;
import info.dgjones.abora.gold.negoti8.ProtocolBroker;
import info.dgjones.abora.gold.negoti8.ProtocolItem;
import info.dgjones.abora.gold.nkernel.FeActualDataHolder;
import info.dgjones.abora.gold.nkernel.FeActualPlaceHolder;
import info.dgjones.abora.gold.nkernel.FeAdminer;
import info.dgjones.abora.gold.nkernel.FeArrayBundle;
import info.dgjones.abora.gold.nkernel.FeBundle;
import info.dgjones.abora.gold.nkernel.FeClub;
import info.dgjones.abora.gold.nkernel.FeDataHolder;
import info.dgjones.abora.gold.nkernel.FeEdition;
import info.dgjones.abora.gold.nkernel.FeElementBundle;
import info.dgjones.abora.gold.nkernel.FeGrandPlaceHolder;
import info.dgjones.abora.gold.nkernel.FeIDHolder;
import info.dgjones.abora.gold.nkernel.FeKeyMaster;
import info.dgjones.abora.gold.nkernel.FeLabel;
import info.dgjones.abora.gold.nkernel.FePlaceHolder;
import info.dgjones.abora.gold.nkernel.FePlaceHolderBundle;
import info.dgjones.abora.gold.nkernel.FeRangeElement;
import info.dgjones.abora.gold.nkernel.FeServer;
import info.dgjones.abora.gold.nkernel.FeVirtualDataHolder;
import info.dgjones.abora.gold.nkernel.FeVirtualPlaceHolder;
import info.dgjones.abora.gold.nkernel.FeWork;
import info.dgjones.abora.gold.nkernel.RevisionDetectorExecutor;
import info.dgjones.abora.gold.nkernel.StatusDetectorExecutor;
import info.dgjones.abora.gold.nkernel.VolumeTester;
import info.dgjones.abora.gold.nkernel.WorksTestFillDetector;
import info.dgjones.abora.gold.nkernel.WorksTestFillRangeDetector;
import info.dgjones.abora.gold.nkernel.WorksTestStatusDetector;
import info.dgjones.abora.gold.nkernel.WorksTester;
import info.dgjones.abora.gold.nkernel.WorksWaitDetector;
import info.dgjones.abora.gold.nlinks.FeHyperLink;
import info.dgjones.abora.gold.nlinks.FeHyperRef;
import info.dgjones.abora.gold.nlinks.FeMultiRef;
import info.dgjones.abora.gold.nlinks.FePath;
import info.dgjones.abora.gold.nlinks.FeSingleRef;
import info.dgjones.abora.gold.packer.HonestAbePlan;
import info.dgjones.abora.gold.packer.PersistentCleaner;
import info.dgjones.abora.gold.packer.SpareStageSpace;
import info.dgjones.abora.gold.primtab.PrimIndexTable;
import info.dgjones.abora.gold.primtab.PrimIndexTableStepper;
import info.dgjones.abora.gold.primtab.PrimIndexTableTester;
import info.dgjones.abora.gold.primtab.PrimPtr2PtrTable;
import info.dgjones.abora.gold.primtab.PrimPtr2PtrTableStepper;
import info.dgjones.abora.gold.primtab.PrimPtrTable;
import info.dgjones.abora.gold.primtab.PrimPtrTableExecutor;
import info.dgjones.abora.gold.primtab.PrimPtrTableStepper;
import info.dgjones.abora.gold.primtab.PrimPtrTableTester;
import info.dgjones.abora.gold.primtab.PrimRemovedObject;
import info.dgjones.abora.gold.primtab.PrimSet;
import info.dgjones.abora.gold.primtab.PrimSetExecutor;
import info.dgjones.abora.gold.primtab.PrimSetStepper;
import info.dgjones.abora.gold.proman.BHHHHandler;
import info.dgjones.abora.gold.proman.BHHHandler;
import info.dgjones.abora.gold.proman.BHHandler;
import info.dgjones.abora.gold.proman.ByteShuffler;
import info.dgjones.abora.gold.proman.CommFillDetector;
import info.dgjones.abora.gold.proman.CommFillRangeDetector;
import info.dgjones.abora.gold.proman.CommRevisionDetector;
import info.dgjones.abora.gold.proman.CommStatusDetector;
import info.dgjones.abora.gold.proman.CommWaitDetector;
import info.dgjones.abora.gold.proman.DetectorEvent;
import info.dgjones.abora.gold.proman.DoneEvent;
import info.dgjones.abora.gold.proman.ExampleHIHHandler;
import info.dgjones.abora.gold.proman.ExceptionRecord;
import info.dgjones.abora.gold.proman.ExecutePromiseFile;
import info.dgjones.abora.gold.proman.FilledEvent;
import info.dgjones.abora.gold.proman.GrabbedEvent;
import info.dgjones.abora.gold.proman.HHBHandler;
import info.dgjones.abora.gold.proman.HHHBHandler;
import info.dgjones.abora.gold.proman.HHHHHHHHandler;
import info.dgjones.abora.gold.proman.HHHHHHHandler;
import info.dgjones.abora.gold.proman.HHHHHHandler;
import info.dgjones.abora.gold.proman.HHHHHandler;
import info.dgjones.abora.gold.proman.HHHHandler;
import info.dgjones.abora.gold.proman.HHHandler;
import info.dgjones.abora.gold.proman.HHandler;
import info.dgjones.abora.gold.proman.NoShuffler;
import info.dgjones.abora.gold.proman.PacketPortal;
import info.dgjones.abora.gold.proman.PairPortal;
import info.dgjones.abora.gold.proman.Portal;
import info.dgjones.abora.gold.proman.PromiseManager;
import info.dgjones.abora.gold.proman.RangeFilledEvent;
import info.dgjones.abora.gold.proman.ReleasedEvent;
import info.dgjones.abora.gold.proman.RequestHandler;
import info.dgjones.abora.gold.proman.RevisedEvent;
import info.dgjones.abora.gold.proman.SimpleShuffler;
import info.dgjones.abora.gold.proman.SpecialHandler;
import info.dgjones.abora.gold.proman.VHBHandler;
import info.dgjones.abora.gold.proman.VHHHHHHandler;
import info.dgjones.abora.gold.proman.VHHHHHandler;
import info.dgjones.abora.gold.proman.VHHHHandler;
import info.dgjones.abora.gold.proman.VHHHandler;
import info.dgjones.abora.gold.proman.VHHandler;
import info.dgjones.abora.gold.props.BertPropChange;
import info.dgjones.abora.gold.props.CannotPartializeChange;
import info.dgjones.abora.gold.props.DetectorWaitingChange;
import info.dgjones.abora.gold.props.EndorsementsChange;
import info.dgjones.abora.gold.props.FullPropChange;
import info.dgjones.abora.gold.props.PermissionsChange;
import info.dgjones.abora.gold.props.PropChange;
import info.dgjones.abora.gold.props.SensorPropChange;
import info.dgjones.abora.gold.purging.LiberalPurgeror;
import info.dgjones.abora.gold.rcmain.FDListener;
import info.dgjones.abora.gold.rcmain.IPPromiseListener;
import info.dgjones.abora.gold.rcmain.IPRendezvousListener;
import info.dgjones.abora.gold.rcmain.MainDummy;
import info.dgjones.abora.gold.rcmain.SelectServerLoop;
import info.dgjones.abora.gold.rcmain.ServerChunk;
import info.dgjones.abora.gold.rcmain.ServerLoop;
import info.dgjones.abora.gold.rcmain.SetCommProtocol;
import info.dgjones.abora.gold.rcmain.SetDiskProtocol;
import info.dgjones.abora.gold.schunk.ChunkCleaner;
import info.dgjones.abora.gold.set.SHTO;
import info.dgjones.abora.gold.settab.SetTableStepper;
import info.dgjones.abora.gold.sheph.ShepherdLockTester;
import info.dgjones.abora.gold.sheph.ShepherdLocked;
import info.dgjones.abora.gold.snarf.Abraham;
import info.dgjones.abora.gold.snarf.CBlockTracker;
import info.dgjones.abora.gold.snarf.CBlockTrackingPacker;
import info.dgjones.abora.gold.snarf.DiskCountSpecialist;
import info.dgjones.abora.gold.snarf.DiskManager;
import info.dgjones.abora.gold.snarf.DiskPurgeRate;
import info.dgjones.abora.gold.snarf.DoublingFlock;
import info.dgjones.abora.gold.snarf.FakePacker;
import info.dgjones.abora.gold.snarf.FlockInfo;
import info.dgjones.abora.gold.snarf.FlockLocation;
import info.dgjones.abora.gold.snarf.MockTurtle;
import info.dgjones.abora.gold.snarf.PairFlock;
import info.dgjones.abora.gold.snarf.Pumpkin;
import info.dgjones.abora.gold.snarf.Purgeror;
import info.dgjones.abora.gold.snarf.SimpleTurtle;
import info.dgjones.abora.gold.snarf.SnarfHandler;
import info.dgjones.abora.gold.snarf.SnarfPacker;
import info.dgjones.abora.gold.snarf.SnarfRecord;
import info.dgjones.abora.gold.snarf.TestFlockInfo;
import info.dgjones.abora.gold.snarf.TestPacker;
import info.dgjones.abora.gold.snarf.Turtle;
import info.dgjones.abora.gold.snfinfo.SnarfStatistics;
import info.dgjones.abora.gold.snfinfo.SpecialistRcvrJig;
import info.dgjones.abora.gold.spaces.CompositeMapping;
import info.dgjones.abora.gold.spaces.ConstantMapping;
import info.dgjones.abora.gold.spaces.EmptyMapping;
import info.dgjones.abora.gold.spaces.basic.BasicSpace;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.Dsp;
import info.dgjones.abora.gold.spaces.basic.FilterTester;
import info.dgjones.abora.gold.spaces.basic.IDTester;
import info.dgjones.abora.gold.spaces.basic.IntegerUpOrder;
import info.dgjones.abora.gold.spaces.basic.Mapping;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.RealTester;
import info.dgjones.abora.gold.spaces.basic.ReverseOrder;
import info.dgjones.abora.gold.spaces.basic.SequenceTester;
import info.dgjones.abora.gold.spaces.basic.SimpleMapping;
import info.dgjones.abora.gold.spaces.basic.UnOrdered;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.cross.ActualTuple;
import info.dgjones.abora.gold.spaces.cross.BoxProjectionStepper;
import info.dgjones.abora.gold.spaces.cross.BoxStepper;
import info.dgjones.abora.gold.spaces.cross.CrossRegion;
import info.dgjones.abora.gold.spaces.cross.CrossSpace;
import info.dgjones.abora.gold.spaces.cross.GenericCrossRegion;
import info.dgjones.abora.gold.spaces.cross.GenericCrossSpace;
import info.dgjones.abora.gold.spaces.cross.MergeStepper;
import info.dgjones.abora.gold.spaces.cross.PtrArrayStepper;
import info.dgjones.abora.gold.spaces.cross.Tuple;
import info.dgjones.abora.gold.spaces.integers.AscendingIntegerStepper;
import info.dgjones.abora.gold.spaces.integers.DescendingIntegerStepper;
import info.dgjones.abora.gold.spaces.integers.IntegerArrangement;
import info.dgjones.abora.gold.spaces.integers.IntegerEdgeAccumulator;
import info.dgjones.abora.gold.spaces.integers.IntegerEdgeStepper;
import info.dgjones.abora.gold.spaces.integers.IntegerMapping;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.spaces.integers.IntegerRegion;
import info.dgjones.abora.gold.spaces.integers.IntegerRegionTester;
import info.dgjones.abora.gold.spaces.integers.IntegerSimpleRegionStepper;
import info.dgjones.abora.gold.spaces.integers.IntegerSpace;
import info.dgjones.abora.gold.spaces.integers.RegionTester;
import info.dgjones.abora.gold.spaces.unordered.HeaperAsPosition;
import info.dgjones.abora.gold.spaces.unordered.HeaperDsp;
import info.dgjones.abora.gold.spaces.unordered.HeaperSpace;
import info.dgjones.abora.gold.spaces.unordered.IDDsp;
import info.dgjones.abora.gold.spaces.unordered.IDSpace;
import info.dgjones.abora.gold.spaces.unordered.IdentityDsp;
import info.dgjones.abora.gold.spaces.unordered.SetRegion;
import info.dgjones.abora.gold.spaces.unordered.StrongAsPosition;
import info.dgjones.abora.gold.srvloop.ListenerEmulsion;
import info.dgjones.abora.gold.srvloop.TestChunk;
import info.dgjones.abora.gold.stacker.StackExaminer;
import info.dgjones.abora.gold.stepper.EmptyStepper;
import info.dgjones.abora.gold.stepper.ItemStepper;
import info.dgjones.abora.gold.sysadm.FeArchiver;
import info.dgjones.abora.gold.tabent.TableEntryTester;
import info.dgjones.abora.gold.tabtool.LPPrimeSizeProvider;
import info.dgjones.abora.gold.tabtool.PrimeSizeProvider;
import info.dgjones.abora.gold.tclude.TrailBlazer;
import info.dgjones.abora.gold.testing.HashSetTester;
import info.dgjones.abora.gold.testing.HashTableTester;
import info.dgjones.abora.gold.testing.HelloTester;
import info.dgjones.abora.gold.testing.ImmuSetTester;
import info.dgjones.abora.gold.testing.IntegerTableTester;
import info.dgjones.abora.gold.testing.MuSetTester;
import info.dgjones.abora.gold.testing.ScruSetTester;
import info.dgjones.abora.gold.testing.SetTester;
import info.dgjones.abora.gold.testing.Tester;
import info.dgjones.abora.gold.tokens.TokenSource;
import info.dgjones.abora.gold.traces.BoundedTrace;
import info.dgjones.abora.gold.traces.BranchDescription;
import info.dgjones.abora.gold.traces.DagBranch;
import info.dgjones.abora.gold.traces.DagWood;
import info.dgjones.abora.gold.traces.RootBranch;
import info.dgjones.abora.gold.traces.TracePosition;
import info.dgjones.abora.gold.traces.TreeBranch;
import info.dgjones.abora.gold.tumbler.AfterReal;
import info.dgjones.abora.gold.tumbler.AfterSequence;
import info.dgjones.abora.gold.tumbler.BeforeReal;
import info.dgjones.abora.gold.tumbler.BeforeSequence;
import info.dgjones.abora.gold.tumbler.BeforeSequencePrefix;
import info.dgjones.abora.gold.tumbler.ExplicitArrangement;
import info.dgjones.abora.gold.tumbler.IDUpOrder;
import info.dgjones.abora.gold.tumbler.IEEE32Pos;
import info.dgjones.abora.gold.tumbler.IEEE64Pos;
import info.dgjones.abora.gold.tumbler.IEEE8Pos;
import info.dgjones.abora.gold.tumbler.RealEdge;
import info.dgjones.abora.gold.tumbler.RealManager;
import info.dgjones.abora.gold.tumbler.RealPos;
import info.dgjones.abora.gold.tumbler.RealRegion;
import info.dgjones.abora.gold.tumbler.RealSpace;
import info.dgjones.abora.gold.tumbler.RealUpOrder;
import info.dgjones.abora.gold.tumbler.Sequence;
import info.dgjones.abora.gold.tumbler.SequenceEdge;
import info.dgjones.abora.gold.tumbler.SequenceManager;
import info.dgjones.abora.gold.tumbler.SequenceMapping;
import info.dgjones.abora.gold.tumbler.SequenceRegion;
import info.dgjones.abora.gold.tumbler.SequenceSpace;
import info.dgjones.abora.gold.tumbler.SequenceUpOrder;
import info.dgjones.abora.gold.turtle.ActualPropChanger;
import info.dgjones.abora.gold.turtle.Agenda;
import info.dgjones.abora.gold.turtle.AgendaItem;
import info.dgjones.abora.gold.turtle.HeightChanger;
import info.dgjones.abora.gold.turtle.Matcher;
import info.dgjones.abora.gold.turtle.NorthRecorderChecker;
import info.dgjones.abora.gold.turtle.PropChanger;
import info.dgjones.abora.gold.turtle.RecorderHoister;
import info.dgjones.abora.gold.turtle.RecorderTrigger;
import info.dgjones.abora.gold.turtle.Sequencer;
import info.dgjones.abora.gold.turtle.SouthRecorderChecker;
import info.dgjones.abora.gold.urdi.CountStream;
import info.dgjones.abora.gold.urdi.HashStream;
import info.dgjones.abora.gold.urdi.ReadArrayStream;
import info.dgjones.abora.gold.urdi.ReadMemStream;
import info.dgjones.abora.gold.urdi.SnarfInfoHandler;
import info.dgjones.abora.gold.urdi.WriteArrayStream;
import info.dgjones.abora.gold.urdi.WriteMemStream;
import info.dgjones.abora.gold.urdi.WriteVariableArrayStream;
import info.dgjones.abora.gold.wrapper.FeAbstractWrapperDef;
import info.dgjones.abora.gold.wrapper.FeAbstractWrapperSpec;
import info.dgjones.abora.gold.wrapper.FeConcreteWrapperSpec;
import info.dgjones.abora.gold.wrapper.FeDirectWrapperDef;
import info.dgjones.abora.gold.wrapper.FeDirectWrapperSpec;
import info.dgjones.abora.gold.wrapper.FeIndirectWrapperDef;
import info.dgjones.abora.gold.wrapper.FeIndirectWrapperSpec;
import info.dgjones.abora.gold.wrapper.FeSet;
import info.dgjones.abora.gold.wrapper.FeText;
import info.dgjones.abora.gold.wrapper.FeWorkSet;
import info.dgjones.abora.gold.wrapper.FeWrapper;
import info.dgjones.abora.gold.wrapper.FeWrapperDef;
import info.dgjones.abora.gold.wrapper.FeWrapperSpec;
import info.dgjones.abora.gold.x.PrimFloatSpec;
import info.dgjones.abora.gold.x.PrimFloatValue;
import info.dgjones.abora.gold.x.PrimIEEE32;
import info.dgjones.abora.gold.x.PrimIEEE64;
import info.dgjones.abora.gold.x.PrimIntValue;
import info.dgjones.abora.gold.x.PrimIntegerSpec;
import info.dgjones.abora.gold.x.PrimPointerSpec;
import info.dgjones.abora.gold.x.PrimSpec;
import info.dgjones.abora.gold.x.PrimValue;
import info.dgjones.abora.gold.xcvr.Binary2Rcvr;
import info.dgjones.abora.gold.xcvr.Binary2XcvrMaker;
import info.dgjones.abora.gold.xcvr.Binary2Xmtr;
import info.dgjones.abora.gold.xcvr.BogusXcvrMaker;
import info.dgjones.abora.gold.xcvr.CommIbid;
import info.dgjones.abora.gold.xcvr.DiskIniter;
import info.dgjones.abora.gold.xcvr.DiskSpecialist;
import info.dgjones.abora.gold.xcvr.FakeDisk;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Recipe;
import info.dgjones.abora.gold.xcvr.ShuffleTester;
import info.dgjones.abora.gold.xcvr.SpecialistRcvr;
import info.dgjones.abora.gold.xcvr.SpecialistXmtr;
import info.dgjones.abora.gold.xcvr.TextyRcvr;
import info.dgjones.abora.gold.xcvr.TextyXcvrMaker;
import info.dgjones.abora.gold.xcvr.TextyXmtr;
import info.dgjones.abora.gold.xcvr.TransferGeneralist;
import info.dgjones.abora.gold.xcvr.TransferSpecialist;
import info.dgjones.abora.gold.xcvr.XcvrMaker;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xcvr.XnBufferedReadStream;
import info.dgjones.abora.gold.xcvr.XnBufferedWriteStream;
import info.dgjones.abora.gold.xcvr.XnReadStream;
import info.dgjones.abora.gold.xcvr.XnWriteStream;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import info.dgjones.abora.gold.xpp.become.BecomeTester;
import java.io.PrintWriter;

public class XnBufferedWriteStream extends XnWriteStream {

	protected PacketPortal myPortal;
	protected UInt8Array myBuffer;
	protected int myNext;
/*
udanax-top.st:70635:
XnWriteStream subclass: #XnBufferedWriteStream
	instanceVariableNames: '
		myPortal {PacketPortal}
		myBuffer {UInt8Array}
		myNext {Int32}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Xcvr'!
*/
/*
udanax-top.st:70642:
(XnBufferedWriteStream getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:70711:
(CxxSystemOrganization fileNamed: 'bootpln')
	comment: ''!
*/
/*
udanax-top.st:70713:
(CxxSystemOrganization getOrMakeFileNamed: 'bootpln')
	addClass: BootPlan
getOrMakeCxxClassDescription in: #public;
	addClass: Connection
getOrMakeCxxClassDescription in: #public;
	addClass: BootMaker
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:70722:
(CxxSystemOrganization getOrMakeFileNamed: 'bootpln')
	addClass: NestedConnection
getOrMakeCxxClassDescription in: #protected!
*/
/*
udanax-top.st:70727:
(CxxSystemOrganization getOrMakeFileNamed: 'bootpln')
	addClass: ClearPlan
getOrMakeCxxClassDescription in: #private;
	addClass: DirectConnection
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:70734:
(CxxSystemOrganization fileNamed: 'comdtct')
	comment: ''!
*/
/*
udanax-top.st:70737:
(CxxSystemOrganization getOrMakeFileNamed: 'comdtct')
	addClass: CommWaitDetector
getOrMakeCxxClassDescription in: #protected;
	addClass: DetectorEvent
getOrMakeCxxClassDescription in: #protected;
	addClass: CommRevisionDetector
getOrMakeCxxClassDescription in: #protected;
	addClass: CommFillRangeDetector
getOrMakeCxxClassDescription in: #protected;
	addClass: CommFillDetector
getOrMakeCxxClassDescription in: #protected;
	addClass: CommStatusDetector
getOrMakeCxxClassDescription in: #protected!
*/
/*
udanax-top.st:70752:
(CxxSystemOrganization getOrMakeFileNamed: 'comdtct')
	cxxHeader: '#include "shephx.hxx"' in: #private!
*/
/*
udanax-top.st:70755:
(CxxSystemOrganization getOrMakeFileNamed: 'comdtct')
	addClass: FilledEvent
getOrMakeCxxClassDescription in: #private;
	addClass: GrabbedEvent
getOrMakeCxxClassDescription in: #private;
	addClass: DoneEvent
getOrMakeCxxClassDescription in: #private;
	addClass: RangeFilledEvent
getOrMakeCxxClassDescription in: #private;
	addClass: ReleasedEvent
getOrMakeCxxClassDescription in: #private;
	addClass: RevisedEvent
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:70769:
(CxxSystemOrganization fileNamed: 'handlrs')
	comment: ''!
*/
/*
udanax-top.st:70772:
(CxxSystemOrganization getOrMakeFileNamed: 'handlrs')
	hxxHeader: 'typedef void (*VHHFn) (APTR(Heaper), APTR(Heaper));
typedef void (*VHHHFn) (APTR(Heaper), APTR(Heaper), APTR(Heaper));
typedef void (*VHFn) (APTR(Heaper));
typedef void (*VHHHHFn) (APTR(Heaper), APTR(Heaper), APTR(Heaper), APTR(Heaper));
typedef void (*VHHHHHFn) (APTR(Heaper), APTR(Heaper), APTR(Heaper), APTR(Heaper), APTR(Heaper));
typedef void (*VHBFn) (APTR(Heaper), BooleanVar);
typedef SPTR(Heaper) (*HFn) ();
typedef SPTR(Heaper) (*HHFn) (APTR(Heaper));
typedef SPTR(Heaper) (*HHHFn) (APTR(Heaper), APTR(Heaper));
typedef SPTR(Heaper) (*HHHHFn) (APTR(Heaper), APTR(Heaper), APTR(Heaper));
typedef SPTR(Heaper) (*HHHHHFn) (APTR(Heaper), APTR(Heaper), APTR(Heaper), APTR(Heaper));
typedef SPTR(Heaper) (*HHHHHHFn) (APTR(Heaper), APTR(Heaper), APTR(Heaper), APTR(Heaper), APTR(Heaper));
typedef SPTR(Heaper) (*HHHHHHHFn) (APTR(Heaper), APTR(Heaper), APTR(Heaper), APTR(Heaper), APTR(Heaper), APTR(Heaper));
typedef BooleanVar (*BHHFn) (APTR(Heaper), APTR(Heaper));
typedef BooleanVar (*BHFn) (APTR(Heaper));
typedef SPTR(Heaper) (*HHBFn) (APTR(Heaper), BooleanVar);
typedef SPTR(Heaper) (*HHHBFn) (APTR(Heaper), APTR(Heaper), BooleanVar);
typedef BooleanVar (*BHHHFn) (APTR(Heaper), APTR(Heaper), APTR(Heaper));
' in: #public!
*/
/*
udanax-top.st:70793:
(CxxSystemOrganization getOrMakeFileNamed: 'handlrs')
	addClass: RequestHandler
getOrMakeCxxClassDescription in: #public;
	addClass: HHHHHHHHandler
getOrMakeCxxClassDescription in: #public;
	addClass: BHHandler
getOrMakeCxxClassDescription in: #public;
	addClass: HHBHandler
getOrMakeCxxClassDescription in: #public;
	addClass: HHHHandler
getOrMakeCxxClassDescription in: #public;
	addClass: HHHBHandler
getOrMakeCxxClassDescription in: #public;
	addClass: BHHHandler
getOrMakeCxxClassDescription in: #public;
	addClass: HHHHHHHandler
getOrMakeCxxClassDescription in: #public;
	addClass: VHBHandler
getOrMakeCxxClassDescription in: #public;
	addClass: BHHHHandler
getOrMakeCxxClassDescription in: #public;
	addClass: HHandler
getOrMakeCxxClassDescription in: #public;
	addClass: HHHandler
getOrMakeCxxClassDescription in: #public;
	addClass: VHHandler
getOrMakeCxxClassDescription in: #public;
	addClass: VHHHHHHandler
getOrMakeCxxClassDescription in: #public;
	addClass: HHHHHHandler
getOrMakeCxxClassDescription in: #public;
	addClass: VHHHandler
getOrMakeCxxClassDescription in: #public;
	addClass: HHHHHandler
getOrMakeCxxClassDescription in: #public;
	addClass: VHHHHHandler
getOrMakeCxxClassDescription in: #public;
	addClass: VHHHHandler
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:70833:
(CxxSystemOrganization fileNamed: 'memstr')
	comment: ''!
*/
/*
udanax-top.st:70836:
(CxxSystemOrganization fileNamed: 'negoti8')
	comment: ''!
*/
/*
udanax-top.st:70839:
(CxxSystemOrganization getOrMakeFileNamed: 'negoti8')
	addClass: ProtocolBroker
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:70844:
(CxxSystemOrganization getOrMakeFileNamed: 'negoti8')
	addClass: ProtocolItem
getOrMakeCxxClassDescription in: #private;
	addClass: SetDiskProtocol
getOrMakeCxxClassDescription in: #private;
	addClass: SetCommProtocol
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:70853:
(CxxSystemOrganization fileNamed: 'nscotty')
	comment: ''!
*/
/*
udanax-top.st:70856:
(CxxSystemOrganization getOrMakeFileNamed: 'nscotty')
	hxxHeader: '' in: #public!
*/
/*
udanax-top.st:70859:
(CxxSystemOrganization getOrMakeFileNamed: 'nscotty')
	cxxHeader: '#include <stdlib.h>
' in: #public!
*/
/*
udanax-top.st:70862:
(CxxSystemOrganization getOrMakeFileNamed: 'nscotty')
	addClass: XnWriteStream
getOrMakeCxxClassDescription in: #public;
	addClass: WriteVariableArrayStream
getOrMakeCxxClassDescription in: #public;
	addClass: XnReadStream
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:70870:
(CxxSystemOrganization getOrMakeFileNamed: 'nscotty')
	addClass: WriteMemStream
getOrMakeCxxClassDescription in: #private;
	addClass: ReadArrayStream
getOrMakeCxxClassDescription in: #private;
	addClass: WriteArrayStream
getOrMakeCxxClassDescription in: #private;
	addClass: ReadMemStream
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:70881:
(CxxSystemOrganization fileNamed: 'portal')
	comment: ''!
*/
/*
udanax-top.st:70884:
(CxxSystemOrganization getOrMakeFileNamed: 'portal')
	addClass: Portal
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:70889:
(CxxSystemOrganization getOrMakeFileNamed: 'portal')
	addClass: PacketPortal
getOrMakeCxxClassDescription in: #protected;
	addClass: XnBufferedReadStream
getOrMakeCxxClassDescription in: #protected;
	addClass: PairPortal
getOrMakeCxxClassDescription in: #protected!
*/
/*
udanax-top.st:70898:
(CxxSystemOrganization getOrMakeFileNamed: 'portal')
	addClass: XnBufferedWriteStream
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:70903:
(CxxSystemOrganization fileNamed: 'prolstn')
	comment: ''!
*/
/*
udanax-top.st:70906:
(CxxSystemOrganization getOrMakeFileNamed: 'prolstn')
	cxxHeader: '#ifdef unix
#	include "sys/ioctl.h"
#	ifndef __sgi
#		include "sys/filio.h"
#	else
#		include <osfcn.h>
#	endif
#endif
' in: #public!
*/
/*
udanax-top.st:70917:
(CxxSystemOrganization getOrMakeFileNamed: 'prolstn')
	addClass: IPPromiseListener
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:70921:
(CxxSystemOrganization getOrMakeFileNamed: 'prolstn')
	addClass: FePromiseSession
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:70926:
(CxxSystemOrganization fileNamed: 'proman')
	comment: ''!
*/
/*
udanax-top.st:70929:
(CxxSystemOrganization getOrMakeFileNamed: 'proman')
	hxxHeader: 'typedef void (*VHFn) (APTR(Heaper));
#define ERRLIST_10(a,b,c,d,e,f,g,h,i,j)	STR(a),ERRLIST_9(b,c,d,e,f,g,h,i,j)
#include "loggerx.hxx"' in: #public!
*/
/*
udanax-top.st:70934:
(CxxSystemOrganization getOrMakeFileNamed: 'proman')
	addClass: ExampleHIHHandler
getOrMakeCxxClassDescription in: #public;
	addClass: PromiseManager
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:70940:
(CxxSystemOrganization getOrMakeFileNamed: 'proman')
	addClass: ByteShuffler
getOrMakeCxxClassDescription in: #protected;
	addClass: ExecutePromiseFile
getOrMakeCxxClassDescription in: #protected!
*/
/*
udanax-top.st:70947:
(CxxSystemOrganization getOrMakeFileNamed: 'proman')
	cxxHeader: '#include "shephx.hxx"
' in: #private!
*/
/*
udanax-top.st:70951:
(CxxSystemOrganization getOrMakeFileNamed: 'proman')
	addClass: NoShuffler
getOrMakeCxxClassDescription in: #private;
	addClass: SimpleShuffler
getOrMakeCxxClassDescription in: #private;
	addClass: ExceptionRecord
getOrMakeCxxClassDescription in: #private;
	addClass: SpecialHandler
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:70961:
(CxxSystemOrganization getOrMakeFileNamed: 'proman')
	addClass: ShuffleTester
getOrMakeCxxClassDescription in: #test!
*/
/*
udanax-top.st:70966:
(CxxSystemOrganization fileNamed: 'schunk')
	comment: ''!
*/
/*
udanax-top.st:70969:
(CxxSystemOrganization getOrMakeFileNamed: 'schunk')
	cxxHeader: '#include <stdlib.h>
#include "allocx.hxx"' in: #public!
*/
/*
udanax-top.st:70973:
(CxxSystemOrganization getOrMakeFileNamed: 'schunk')
	addClass: ServerChunk
getOrMakeCxxClassDescription in: #public;
	addClass: ChunkCleaner
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:70979:
(CxxSystemOrganization getOrMakeFileNamed: 'schunk')
	addClass: ListenerEmulsion
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:70984:
(CxxSystemOrganization getOrMakeFileNamed: 'schunk')
	addClass: TestChunk
getOrMakeCxxClassDescription in: #test!
*/
/*
udanax-top.st:70989:
(CxxSystemOrganization fileNamed: 'sktsrv')
	comment: ''!
*/
/*
udanax-top.st:70992:
(CxxSystemOrganization getOrMakeFileNamed: 'sktsrv')
	cxxHeader: '#include <sys/types.h>
#if defined(HIGHC) | defined(_MSC_VER)
#	include <sys/time.h>
#endif /- HIGHC | _MSC_VER -/
#include <stdlib.h>
#include <stream.h>
#include <string.h>
#include <fcntl.h>
#ifdef unix
#	include <netdbx.hxx>
#	include <sys/socket.h>
#	include <netinet/in.h>
#	include <sys/socket.h>
#	include <osfcn.h>
#	ifdef __sgi
		int	getdtablesize();		/- SGI forgot to put it in osfcn.h -/
#		include <libc.h>	/- for bzero(), which is called by FD_ZERO -/
#		include <errno.h>
#	endif	/- sgi -/
#endif /- unix -/
#include <signal.h>
#ifdef WIN32
#	include <winsock.h>
#	include <io.h>
#	define close _close
#endif /- WIN32 -/
#ifdef HIGHC
extern "C" {
#	define NOMEMMGR /- TO AVOID GPTR MACRO CONFLICT -/
#	include <nmpcip.h>
};
#endif /- HIGHC -/
#include <socketx.hxx>
' in: #public!
*/
/*
udanax-top.st:71032:
(CxxSystemOrganization getOrMakeFileNamed: 'sktsrv')
	addClass: FDListener
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:71036:
(CxxSystemOrganization getOrMakeFileNamed: 'sktsrv')
	hxxHeader: '#ifdef WIN32
#	include <fdset.h>
#else
#ifdef unix
#	include <sys/time.h>
#	include <unistd.h>
#endif /- unix -/
#endif /- WIN32 -/
#include <sys/types.h>' in: #private!
*/
/*
udanax-top.st:71047:
(CxxSystemOrganization getOrMakeFileNamed: 'sktsrv')
	cxxHeader: '#ifdef WIN32
#	include <io.h>
#	include <winsock.h>
#else
#ifdef unix
#	ifndef __sgi
#		include <sys/filio.h>
#	endif /- __sgi -/
#endif /- unix -/
#endif /- WIN32 -/' in: #private!
*/
/*
udanax-top.st:71058:
(CxxSystemOrganization getOrMakeFileNamed: 'sktsrv')
	addClass: SelectServerLoop
getOrMakeCxxClassDescription in: #private;
	addClass: IPRendezvousListener
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:71064:
(CxxSystemOrganization fileNamed: 'srvloop')
	comment: ''!
*/
/*
udanax-top.st:71067:
(CxxSystemOrganization getOrMakeFileNamed: 'srvloop')
	hxxHeader: '' in: #public!
*/
/*
udanax-top.st:71070:
(CxxSystemOrganization getOrMakeFileNamed: 'srvloop')
	cxxHeader: '#include <stdlib.h>' in: #public!
*/
/*
udanax-top.st:71072:
(CxxSystemOrganization getOrMakeFileNamed: 'srvloop')
	addClass: ServerLoop
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:71076:
(CxxSystemOrganization fileNamed: 'bin2com')
	comment: ''!
*/
/*
udanax-top.st:71079:
(CxxSystemOrganization getOrMakeFileNamed: 'bin2com')
	addClass: Binary2XcvrMaker
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:71084:
(CxxSystemOrganization getOrMakeFileNamed: 'bin2com')
	addClass: Binary2Xmtr
getOrMakeCxxClassDescription in: #private;
	addClass: Binary2Rcvr
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:71091:
(CxxSystemOrganization fileNamed: 'cookbk')
	comment: ''!
*/
/*
udanax-top.st:71094:
(CxxSystemOrganization getOrMakeFileNamed: 'cookbk')
	addClass: Cookbook
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:71099:
(CxxSystemOrganization getOrMakeFileNamed: 'cookbk')
	addClass: ActualCookbook
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:71104:
(CxxSystemOrganization fileNamed: 'nxcvr')
	comment: ''!
*/
/*
udanax-top.st:71107:
(CxxSystemOrganization getOrMakeFileNamed: 'nxcvr')
	hxxHeader: '' in: #public!
*/
/*
udanax-top.st:71110:
(CxxSystemOrganization getOrMakeFileNamed: 'nxcvr')
	addClass: Rcvr
getOrMakeCxxClassDescription in: #public;
	addClass: Xmtr
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:71116:
(CxxSystemOrganization fileNamed: 'recipe')
	comment: ''!
*/
/*
udanax-top.st:71119:
(CxxSystemOrganization getOrMakeFileNamed: 'recipe')
	hxxHeader: '#include "parrayx.oxx" // definition for friend function in Recipe' in: #public!
*/
/*
udanax-top.st:71122:
(CxxSystemOrganization getOrMakeFileNamed: 'recipe')
	addClass: Recipe
getOrMakeCxxClassDescription in: #public;
	addClass: CopyRecipe
getOrMakeCxxClassDescription in: #public;
	addClass: StubRecipe
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:71130:
(CxxSystemOrganization fileNamed: 'txtcomm')
	comment: ''!
*/
/*
udanax-top.st:71133:
(CxxSystemOrganization getOrMakeFileNamed: 'txtcomm')
	hxxHeader: '#define CONVERTSTRLEN 16' in: #public!
*/
/*
udanax-top.st:71136:
(CxxSystemOrganization getOrMakeFileNamed: 'txtcomm')
	addClass: TextyXcvrMaker
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:71140:
(CxxSystemOrganization getOrMakeFileNamed: 'txtcomm')
	cxxHeader: '#include <string.h>
#include <ctype.h>' in: #private!
*/
/*
udanax-top.st:71144:
(CxxSystemOrganization getOrMakeFileNamed: 'txtcomm')
	addClass: TextyRcvr
getOrMakeCxxClassDescription in: #private;
	addClass: TextyXmtr
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:71150:
(CxxSystemOrganization fileNamed: 'xfrspec')
	comment: ''!
*/
/*
udanax-top.st:71153:
(CxxSystemOrganization getOrMakeFileNamed: 'xfrspec')
	hxxHeader: '#include "choosex.hxx"' in: #public!
*/
/*
udanax-top.st:71156:
(CxxSystemOrganization getOrMakeFileNamed: 'xfrspec')
	cxxHeader: '#include "tofup.hxx"
#include "fhashx.hxx"
#include <string.h>' in: #public!
*/
/*
udanax-top.st:71160:
(CxxSystemOrganization getOrMakeFileNamed: 'xfrspec')
	addClass: XcvrMaker
getOrMakeCxxClassDescription in: #public;
	addClass: SpecialistRcvr
getOrMakeCxxClassDescription in: #public;
	addClass: TransferSpecialist
getOrMakeCxxClassDescription in: #public;
	addClass: SpecialistXmtr
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:71170:
(CxxSystemOrganization getOrMakeFileNamed: 'xfrspec')
	cxxHeader: '#include "copyrcpx.hxx"
extern Recipe * XppCuisine;
' in: #private!
*/
/*
udanax-top.st:71175:
(CxxSystemOrganization getOrMakeFileNamed: 'xfrspec')
	addClass: CommIbid
getOrMakeCxxClassDescription in: #private;
	addClass: BogusXcvrMaker
getOrMakeCxxClassDescription in: #private;
	addClass: TransferGeneralist
getOrMakeCxxClassDescription in: #private;
	addClass: CategoryRecipe
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:71185:
(CxxSystemOrganization fileNamed: 'consist')
	comment: ''!
*/
/*
udanax-top.st:71188:
(CxxSystemOrganization getOrMakeFileNamed: 'consist')
	addClass: CBlockTrackingPacker
getOrMakeCxxClassDescription in: #test;
	addClass: PrintCBlocksTracks
getOrMakeCxxClassDescription in: #test;
	addClass: TrackCBlocks
getOrMakeCxxClassDescription in: #test;
	addClass: CBlockTracker
getOrMakeCxxClassDescription in: #test!
*/
/*
udanax-top.st:71199:
(CxxSystemOrganization fileNamed: 'diskman')
	comment: ''!
*/
/*
udanax-top.st:71202:
(CxxSystemOrganization getOrMakeFileNamed: 'diskman')
	hxxHeader: '#define BEGIN_CONSISTENT(dirty)												\
	{	CurrentPacker.fluidGet()->beginConsistent(dirty);						\
		CurrentPacker.fluidGet()->consistentBlockAt(__FILE__,__LINE__);	\
		PLANT_BOMB(ConsistentBlock,Boom);									\
		ARM_BOMB(Boom,(dirty));												\
		{																		\
			FLUID_BIND(InsideTransactionFlag,TRUE)  {
	
#define END_CONSISTENT	}	}	}
#define BEGIN_INSISTENT(dirty)													\
	{	if (! InsideTransactionFlag.fluidFetch()) {									\
			BLAST(Assertion_failed);												\
		}																			\
		CurrentPacker.fluidGet()->beginConsistent(dirty);						\
		CurrentPacker.fluidGet()->consistentBlockAt(__FILE__,__LINE__);	\
		PLANT_BOMB(ConsistentBlock,Boom);									\
		ARM_BOMB(Boom,(dirty));												\
		{
	
#define END_INSISTENT	}	}
' in: #public!
*/
/*
udanax-top.st:71227:
(CxxSystemOrganization getOrMakeFileNamed: 'diskman')
	cxxHeader: '#include "allocx.hxx"' in: #public!
*/
/*
udanax-top.st:71229:
(CxxSystemOrganization getOrMakeFileNamed: 'diskman')
	addClass: DiskManager
getOrMakeCxxClassDescription in: #public;
	addClass: ShepherdBootMaker
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:71235:
(CxxSystemOrganization getOrMakeFileNamed: 'diskman')
	cxxHeader: '#include <stdlib.h>' in: #private!
*/
/*
udanax-top.st:71238:
(CxxSystemOrganization getOrMakeFileNamed: 'diskman')
	addClass: FromDiskPlan
getOrMakeCxxClassDescription in: #private;
	addClass: DiskManagerEmulsion
getOrMakeCxxClassDescription in: #private;
	addClass: DiskConnection
getOrMakeCxxClassDescription in: #private;
	addClass: Cattleman
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:71248:
(CxxSystemOrganization getOrMakeFileNamed: 'diskman')
	addClass: DiskTester
getOrMakeCxxClassDescription in: #test;
	addClass: MultiCounter
getOrMakeCxxClassDescription in: #test!
*/
/*
udanax-top.st:71255:
(CxxSystemOrganization fileNamed: 'dstat')
	comment: ''!
*/
/*
udanax-top.st:71258:
(CxxSystemOrganization getOrMakeFileNamed: 'dstat')
	addClass: SnarfStatistics
getOrMakeCxxClassDescription in: #private;
	addClass: SpecialistRcvrJig
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:71265:
(CxxSystemOrganization fileNamed: 'fakedsk')
	comment: ''!
*/
/*
udanax-top.st:71268:
(CxxSystemOrganization getOrMakeFileNamed: 'fakedsk')
	addClass: FakeDisk
getOrMakeCxxClassDescription in: #private;
	addClass: FakePacker
getOrMakeCxxClassDescription in: #private;
	addClass: MockTurtle
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:71277:
(CxxSystemOrganization fileNamed: 'flkinfo')
	comment: ''!
*/
/*
udanax-top.st:71280:
(CxxSystemOrganization getOrMakeFileNamed: 'flkinfo')
	addClass: FlockLocation
getOrMakeCxxClassDescription in: #public;
	addClass: FlockInfo
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:71287:
(CxxSystemOrganization fileNamed: 'packer')
	comment: ''!
*/
/*
udanax-top.st:71290:
(CxxSystemOrganization getOrMakeFileNamed: 'packer')
	addClass: SnarfPacker
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:71295:
(CxxSystemOrganization getOrMakeFileNamed: 'packer')
	hxxHeader: '' in: #private!
*/
/*
udanax-top.st:71298:
(CxxSystemOrganization getOrMakeFileNamed: 'packer')
	cxxHeader: '#include <string.h>' in: #private!
*/
/*
udanax-top.st:71300:
(CxxSystemOrganization getOrMakeFileNamed: 'packer')
	addClass: Pumpkin
getOrMakeCxxClassDescription in: #private;
	addClass: PersistentCleaner
getOrMakeCxxClassDescription in: #private;
	addClass: DiskCountSpecialist
getOrMakeCxxClassDescription in: #private;
	addClass: SnarfRecord
getOrMakeCxxClassDescription in: #private;
	addClass: DiskIniter
getOrMakeCxxClassDescription in: #private;
	addClass: SpareStageSpace
getOrMakeCxxClassDescription in: #private;
	addClass: DiskSpecialist
getOrMakeCxxClassDescription in: #private;
	addClass: CountStream
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:71318:
(CxxSystemOrganization getOrMakeFileNamed: 'packer')
	addClass: TestFlockInfo
getOrMakeCxxClassDescription in: #test;
	addClass: Honestly
getOrMakeCxxClassDescription in: #test;
	addClass: PairFlock
getOrMakeCxxClassDescription in: #test;
	addClass: HashStream
getOrMakeCxxClassDescription in: #test;
	addClass: DoublingFlock
getOrMakeCxxClassDescription in: #test;
	addClass: HonestAbePlan
getOrMakeCxxClassDescription in: #test;
	addClass: HonestAbeIniter
getOrMakeCxxClassDescription in: #test;
	addClass: TestPacker
getOrMakeCxxClassDescription in: #test!
*/
/*
udanax-top.st:71337:
(CxxSystemOrganization fileNamed: 'purging')
	comment: ''!
*/
/*
udanax-top.st:71340:
(CxxSystemOrganization getOrMakeFileNamed: 'purging')
	addClass: LiberalPurgeror
getOrMakeCxxClassDescription in: #public;
	addClass: Purgeror
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:71347:
(CxxSystemOrganization getOrMakeFileNamed: 'purging')
	addClass: DiskPurgeRate
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:71352:
(CxxSystemOrganization fileNamed: 'snfinfo')
	comment: ''!
*/
/*
udanax-top.st:71355:
(CxxSystemOrganization getOrMakeFileNamed: 'snfinfo')
	addClass: SnarfInfoHandler
getOrMakeCxxClassDescription in: #public;
	addClass: SnarfHandler
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:71362:
(CxxSystemOrganization fileNamed: 'turtle')
	comment: ''!
*/
/*
udanax-top.st:71365:
(CxxSystemOrganization getOrMakeFileNamed: 'turtle')
	addClass: Turtle
getOrMakeCxxClassDescription in: #public;
	addClass: AgendaItem
getOrMakeCxxClassDescription in: #public;
	addClass: Sequencer
getOrMakeCxxClassDescription in: #public;
	addClass: Agenda
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:71376:
(CxxSystemOrganization getOrMakeFileNamed: 'turtle')
	addClass: SimpleTurtle
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:71381:
(CxxSystemOrganization fileNamed: 'brange1')
	comment: ''!
*/
/*
udanax-top.st:71384:
(CxxSystemOrganization getOrMakeFileNamed: 'brange1')
	addClass: BeCarrier
getOrMakeCxxClassDescription in: #public;
	addClass: BeRangeElement
getOrMakeCxxClassDescription in: #public;
	addClass: BePlaceHolder
getOrMakeCxxClassDescription in: #public;
	addClass: BeLabel
getOrMakeCxxClassDescription in: #public;
	addClass: BeIDHolder
getOrMakeCxxClassDescription in: #public;
	addClass: BeDataHolder
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:71399:
(CxxSystemOrganization getOrMakeFileNamed: 'brange1')
	addClass: FillDetectorExecutor
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:71404:
(CxxSystemOrganization fileNamed: 'brange2')
	comment: ''!
*/
/*
udanax-top.st:71407:
(CxxSystemOrganization getOrMakeFileNamed: 'brange2')
	cxxHeader: '#include "entx.hxx"  // for various fluids.' in: #public!
*/
/*
udanax-top.st:71410:
(CxxSystemOrganization getOrMakeFileNamed: 'brange2')
	addClass: BeWork
getOrMakeCxxClassDescription in: #public;
	addClass: BeClub
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:71416:
(CxxSystemOrganization getOrMakeFileNamed: 'brange2')
	addClass: BeWorkLockExecutor
getOrMakeCxxClassDescription in: #private;
	addClass: UpdateTransitiveSuperClubIDs
getOrMakeCxxClassDescription in: #private;
	addClass: UpdateTransitiveMemberIDs
getOrMakeCxxClassDescription in: #private;
	addClass: RevisionWatcherExecutor
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:71427:
(CxxSystemOrganization fileNamed: 'brange3')
	comment: ''!
*/
/*
udanax-top.st:71430:
(CxxSystemOrganization getOrMakeFileNamed: 'brange3')
	addClass: BeEdition
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:71435:
(CxxSystemOrganization getOrMakeFileNamed: 'brange3')
	addClass: BeEditionDetectorExecutor
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:71440:
(CxxSystemOrganization fileNamed: 'crypto')
	comment: ''!
*/
/*
udanax-top.st:71443:
(CxxSystemOrganization getOrMakeFileNamed: 'crypto')
	hxxHeader: 'typedef SPTR(Encrypter) (*EncrypterConstructor) (APTR(UInt8Array) OR(NULL) publicKey, APTR(UInt8Array) OR(NULL) privateKey);
#define DEFINE_ENCRYPTER(identifier,encryptorClass) {		\
	REQUIRES(Encrypter);			\
	Encrypter::remember(Sequence::string(identifier), encryptorClass::make);			\
}
#define DEFINE_SCRAMBLER(identifier,scrambler) {	\
	REQUIRES(Scrambler);			\
	Scrambler::remember(Sequence::string(identifier), scrambler);				\
}' in: #public!
*/
/*
udanax-top.st:71456:
(CxxSystemOrganization getOrMakeFileNamed: 'crypto')
	addClass: Encrypter
getOrMakeCxxClassDescription in: #public;
	addClass: Scrambler
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:71462:
(CxxSystemOrganization getOrMakeFileNamed: 'crypto')
	addClass: NoScrambler
getOrMakeCxxClassDescription in: #private;
	addClass: EncrypterMaker
getOrMakeCxxClassDescription in: #private;
	addClass: NoEncrypter
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:71471:
(CxxSystemOrganization fileNamed: 'detect')
	comment: ''!
*/
/*
udanax-top.st:71474:
(CxxSystemOrganization getOrMakeFileNamed: 'detect')
	addClass: FeDetector
getOrMakeCxxClassDescription in: #public;
	addClass: FeFillDetector
getOrMakeCxxClassDescription in: #public;
	addClass: FeFillRangeDetector
getOrMakeCxxClassDescription in: #public;
	addClass: FeRevisionDetector
getOrMakeCxxClassDescription in: #public;
	addClass: FeStatusDetector
getOrMakeCxxClassDescription in: #public;
	addClass: FeWaitDetector
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:71489:
(CxxSystemOrganization fileNamed: 'granmap')
	comment: ''!
*/
/*
udanax-top.st:71492:
(CxxSystemOrganization getOrMakeFileNamed: 'granmap')
	addClass: BeGrandMap
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:71497:
(CxxSystemOrganization getOrMakeFileNamed: 'granmap')
	addClass: GrantStepper
getOrMakeCxxClassDescription in: #private;
	addClass: BackendBootMaker
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:71504:
(CxxSystemOrganization fileNamed: 'id')
	comment: ''!
*/
/*
udanax-top.st:71507:
(CxxSystemOrganization getOrMakeFileNamed: 'id')
	addClass: IDRegion
getOrMakeCxxClassDescription in: #public;
	addClass: IDSpace
getOrMakeCxxClassDescription in: #public;
	addClass: IDDsp
getOrMakeCxxClassDescription in: #public;
	addClass: ID
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:71518:
(CxxSystemOrganization getOrMakeFileNamed: 'id')
	addClass: IDUpOrder
getOrMakeCxxClassDescription in: #private;
	addClass: IDStepper
getOrMakeCxxClassDescription in: #private;
	addClass: IDSimpleStepper
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:71527:
(CxxSystemOrganization getOrMakeFileNamed: 'id')
	addClass: IDTester
getOrMakeCxxClassDescription in: #test!
*/
/*
udanax-top.st:71532:
(CxxSystemOrganization fileNamed: 'nadmin')
	comment: ''!
*/
/*
udanax-top.st:71535:
(CxxSystemOrganization getOrMakeFileNamed: 'nadmin')
	addClass: Lock
getOrMakeCxxClassDescription in: #public;
	addClass: FeLockSmith
getOrMakeCxxClassDescription in: #public;
	addClass: MatchLock
getOrMakeCxxClassDescription in: #public;
	addClass: FeClubDescription
getOrMakeCxxClassDescription in: #public;
	addClass: FeBooLockSmith
getOrMakeCxxClassDescription in: #public;
	addClass: MultiLock
getOrMakeCxxClassDescription in: #public;
	addClass: ChallengeLock
getOrMakeCxxClassDescription in: #public;
	addClass: WallLock
getOrMakeCxxClassDescription in: #public;
	addClass: FeChallengeLockSmith
getOrMakeCxxClassDescription in: #public;
	addClass: FeWallLockSmith
getOrMakeCxxClassDescription in: #public;
	addClass: FeSession
getOrMakeCxxClassDescription in: #public;
	addClass: BooLock
getOrMakeCxxClassDescription in: #public;
	addClass: FeMatchLockSmith
getOrMakeCxxClassDescription in: #public;
	addClass: FeMultiLockSmith
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:71566:
(CxxSystemOrganization getOrMakeFileNamed: 'nadmin')
	addClass: DefaultSession
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:71571:
(CxxSystemOrganization fileNamed: 'nkernel')
	comment: ''!
*/
/*
udanax-top.st:71574:
(CxxSystemOrganization getOrMakeFileNamed: 'nkernel')
	hxxHeader: '#define NOACK void' in: #public!
*/
/*
udanax-top.st:71577:
(CxxSystemOrganization getOrMakeFileNamed: 'nkernel')
	cxxHeader: '#include "choosex.hxx"
' in: #public!
*/
/*
udanax-top.st:71580:
(CxxSystemOrganization getOrMakeFileNamed: 'nkernel')
	addClass: FeRangeElement
getOrMakeCxxClassDescription in: #public;
	addClass: FeBundle
getOrMakeCxxClassDescription in: #public;
	addClass: FeLabel
getOrMakeCxxClassDescription in: #public;
	addClass: FeArrayBundle
getOrMakeCxxClassDescription in: #public;
	addClass: FePlaceHolderBundle
getOrMakeCxxClassDescription in: #public;
	addClass: FePlaceHolder
getOrMakeCxxClassDescription in: #public;
	addClass: FeEdition
getOrMakeCxxClassDescription in: #public;
	addClass: FeWork
getOrMakeCxxClassDescription in: #public;
	addClass: FeDataHolder
getOrMakeCxxClassDescription in: #public;
	addClass: FeServer
getOrMakeCxxClassDescription in: #public;
	addClass: FeClub
getOrMakeCxxClassDescription in: #public;
	addClass: FeKeyMaster
getOrMakeCxxClassDescription in: #public;
	addClass: FeIDHolder
getOrMakeCxxClassDescription in: #public;
	addClass: FeElementBundle
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:71610:
(CxxSystemOrganization getOrMakeFileNamed: 'nkernel')
	addClass: FeVirtualDataHolder
getOrMakeCxxClassDescription in: #private;
	addClass: FeGrandPlaceHolder
getOrMakeCxxClassDescription in: #private;
	addClass: RevisionDetectorExecutor
getOrMakeCxxClassDescription in: #private;
	addClass: FeActualPlaceHolder
getOrMakeCxxClassDescription in: #private;
	addClass: EditionStepper
getOrMakeCxxClassDescription in: #private;
	addClass: FeVirtualPlaceHolder
getOrMakeCxxClassDescription in: #private;
	addClass: StatusDetectorExecutor
getOrMakeCxxClassDescription in: #private;
	addClass: FeActualDataHolder
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:71629:
(CxxSystemOrganization getOrMakeFileNamed: 'nkernel')
	addClass: WorksTestFillDetector
getOrMakeCxxClassDescription in: #test;
	addClass: WorksTestStatusDetector
getOrMakeCxxClassDescription in: #test;
	addClass: WorksTester
getOrMakeCxxClassDescription in: #test;
	addClass: VolumeTester
getOrMakeCxxClassDescription in: #test;
	addClass: WorksTestFillRangeDetector
getOrMakeCxxClassDescription in: #test!
*/
/*
udanax-top.st:71642:
(CxxSystemOrganization fileNamed: 'nlinks')
	comment: ''!
*/
/*
udanax-top.st:71645:
(CxxSystemOrganization getOrMakeFileNamed: 'nlinks')
	addClass: FeHyperRef
getOrMakeCxxClassDescription in: #public;
	addClass: FeSingleRef
getOrMakeCxxClassDescription in: #public;
	addClass: FeHyperLink
getOrMakeCxxClassDescription in: #public;
	addClass: FePath
getOrMakeCxxClassDescription in: #public;
	addClass: FeMultiRef
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:71658:
(CxxSystemOrganization fileNamed: 'sysadm')
	comment: ''!
*/
/*
udanax-top.st:71661:
(CxxSystemOrganization getOrMakeFileNamed: 'sysadm')
	addClass: FeArchiver
getOrMakeCxxClassDescription in: #public;
	addClass: FeAdminer
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:71668:
(CxxSystemOrganization fileNamed: 'wrapper')
	comment: ''!
*/
/*
udanax-top.st:71671:
(CxxSystemOrganization getOrMakeFileNamed: 'wrapper')
	hxxHeader: '/- Function pointer types for wrappers -/
typedef void (*FeWrapperSpecHolder) (APTR(FeWrapperSpec));
typedef SPTR(FeWrapper) (*FeDirectWrapperMaker) (APTR(FeEdition));
typedef SPTR(FeWrapper) (*FeIndirectWrapperMaker) (APTR(FeEdition), APTR(FeWrapper));
typedef BooleanVar (*FeDirectWrapperChecker) (APTR(FeEdition));
typedef BooleanVar (*FeIndirectWrapperChecker) (APTR(FeEdition));
#define ABSTRACTWRAPPER(wrapperName,superName,className) \
	REQUIRES(Sequence); \
	REQUIRES(FeWrapperSpec); \
	FeWrapperSpec::registerAbstract (wrapperName, superName, className::setSpec)
#define DIRECTWRAPPER(wrapperName,superName,className) \
	REQUIRES(Sequence); \
	REQUIRES(FeWrapperSpec); \
	FeWrapperSpec::registerDirect (wrapperName, superName, \
		className::makeWrapper, className::check, className::setSpec)
		
#define INDIRECTWRAPPER(wrapperName,superName,innerName,className) \
	REQUIRES(Sequence); \
	REQUIRES(FeWrapperSpec); \
	FeWrapperSpec::registerDirect (wrapperName, superName, innerName, \
		className::makeWrapper, className::check, className::setSpec)
' in: #public!
*/
/*
udanax-top.st:71699:
(CxxSystemOrganization getOrMakeFileNamed: 'wrapper')
	addClass: FeWrapperSpec
getOrMakeCxxClassDescription in: #public;
	addClass: FeWrapper
getOrMakeCxxClassDescription in: #public;
	addClass: FeSet
getOrMakeCxxClassDescription in: #public;
	addClass: FeText
getOrMakeCxxClassDescription in: #public;
	addClass: FeWorkSet
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:71711:
(CxxSystemOrganization getOrMakeFileNamed: 'wrapper')
	addClass: FeConcreteWrapperSpec
getOrMakeCxxClassDescription in: #private;
	addClass: FeIndirectWrapperSpec
getOrMakeCxxClassDescription in: #private;
	addClass: FeDirectWrapperSpec
getOrMakeCxxClassDescription in: #private;
	addClass: FeWrapperDef
getOrMakeCxxClassDescription in: #private;
	addClass: FeAbstractWrapperSpec
getOrMakeCxxClassDescription in: #private;
	addClass: FeAbstractWrapperDef
getOrMakeCxxClassDescription in: #private;
	addClass: FeIndirectWrapperDef
getOrMakeCxxClassDescription in: #private;
	addClass: FeDirectWrapperDef
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:71730:
(CxxSystemOrganization fileNamed: 'sheph')
	comment: ''!
*/
/*
udanax-top.st:71733:
(CxxSystemOrganization getOrMakeFileNamed: 'sheph')
	hxxHeader: 'class ShepFlag {};
#ifdef GNU
extern ShepFlag shepFlag;
#else
const ShepFlag shepFlag;
#endif
#ifndef STUBBLE
/- Prototype Constructor Identification Junk -/
class PCIJ {};
#ifdef GNU
extern PCIJ pcij;
#else
const PCIJ pcij;
#endif
/- Used to identify calls on a constructor intended to be used only  -/
/- to create prototypes for the use of changeClassToThatOf() -/
#define LOCKED(className)						\
  public:								\
    className(PCIJ);							\
  private:
#define DEFERRED_LOCKED(className)					\
  public:								\
    className(PCIJ);							\
  private:
#define NOFAULT
#define NOLOCK
/- ========================================================================== -/
//
// Attribute macros.  (Class must also be a COPY() class.)
//
//	SHEPHERD_PATRIARCH()	tells stubble to generate a shepherd stub and
//				falls through to SHEPHERD_ANCESTOR
//
//	SHEPHERD_ANCESTOR()	generates a constructor for passing the
//				hash to Abraham during stub creation
//
// Rules for their use:
//
//	- All (abstract or concrete) classes which inherit from Abraham
//	  are "shepherds".
//
//	- Shepherds must be COPY() classes.
//
//	- Every concrete shepherd class must either be, or inherit from,
//	  a class with the SHEPHERD_PATRIARCH() attribute (called a
//	  SHEPHERD_PATRIARCH class.)
//
//	- Every class between a SHEPHERD_PATRIARCH and Abraham must have
//	  either a SHEPHERD_PATRIARCH() or a SHEPHERD_ANCESTOR() attribute.
//
//	- (Thus, the SHEPHERD_ANCESTOR() attribute is optional in classes
//	  below the last SHEPHERD_PATRIARCH.  Giving them the attribute
//	  causes extra code to be generated, allowing you to define
//	  another SHEPHERD_PATRIARCH which inherits from them.)
//
//  - In order to make becomeStub faster, I''ve made the stubbing
//    constructor inline.  It has to be here, and not in the sxx file
//    so that subclasses outside of the module defining the ANCESTOR
//    can see it.  Note that for I use the implicit superclass name for
//    the constructor, and therein rely on single inheritance.  An
//    alternative is to have SHEPHERD_ANCESTOR specify its superclass
//		- ech 3-19-92
//
//  - Put back non-inline variant of SHEPHERD_ANCESTOR for use when
//     inlining is turned off.
//		- ech 4-3-92
/- ========================================================================== -/
#define SHEPHERD_PATRIARCH(className,baseClassName)					\
        public: 							\
	   SPTR(Category) getShepherdStubCategory() CONST;			\
	   void becomeStub();								\
	SHEPHERD_ANCESTOR(className,baseClassName)
#ifdef USE_INLINE
#define SHEPHERD_ANCESTOR(className,baseClassName)					\
     protected: 							\
	   inline className(ShepFlag /-aFlag-/, UInt32 aHash, APTR(FlockInfo) info) \
	   			: baseClassName(shepFlag, aHash, info) {}				\
	private:
#else
/- the constructor definitition in this case is in the .sxx file -/
#define SHEPHERD_ANCESTOR(className)					\
     protected: 							\
	   className(ShepFlag aFlag, UInt32 aHash, APTR(FlockInfo) info);		\
	private:
#endif /- USE_INLINE -/
#endif /- STUBBLE -/
' in: #public!
*/
/*
udanax-top.st:71834:
(CxxSystemOrganization getOrMakeFileNamed: 'sheph')
	addClass: Abraham
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:71838:
(CxxSystemOrganization getOrMakeFileNamed: 'sheph')
	addClass: ShepherdLocked
getOrMakeCxxClassDescription in: #test;
	addClass: ShepherdLockTester
getOrMakeCxxClassDescription in: #test!
*/
/*
udanax-top.st:71845:
(CxxSystemOrganization fileNamed: 'tclude')
	comment: ''!
*/
/*
udanax-top.st:71848:
(CxxSystemOrganization getOrMakeFileNamed: 'tclude')
	hxxHeader: '/- Should only be called if I am not extinct. -/
#define BEGIN_REANIMATE(fossil,Type,var)				\
	{							\
		SPTR(Type) var = CAST(Type,(fossil)->secretRecorder());	\
		PLANT_BOMB(ReleaseRecorder,Boom);			\
		ARM_BOMB(Boom,&*(fossil));			\
		{
		
#define END_REANIMATE	}	}' in: #public!
*/
/*
udanax-top.st:71860:
(CxxSystemOrganization getOrMakeFileNamed: 'tclude')
	addClass: RecorderTrigger
getOrMakeCxxClassDescription in: #public;
	addClass: ResultRecorder
getOrMakeCxxClassDescription in: #public;
	addClass: Matcher
getOrMakeCxxClassDescription in: #public;
	addClass: TrailBlazer
getOrMakeCxxClassDescription in: #public;
	addClass: NorthRecorderChecker
getOrMakeCxxClassDescription in: #public;
	addClass: HashSetCache
getOrMakeCxxClassDescription in: #public;
	addClass: RecorderFossil
getOrMakeCxxClassDescription in: #public;
	addClass: SouthRecorderChecker
getOrMakeCxxClassDescription in: #public;
	addClass: WorkRecorder
getOrMakeCxxClassDescription in: #public;
	addClass: EditionRecorder
getOrMakeCxxClassDescription in: #public;
	addClass: RecorderHoister
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:71884:
(CxxSystemOrganization getOrMakeFileNamed: 'tclude')
	addClass: EditionRecorderFossil
getOrMakeCxxClassDescription in: #private;
	addClass: DirectEditionRecorder
getOrMakeCxxClassDescription in: #private;
	addClass: IndirectWorkRecorder
getOrMakeCxxClassDescription in: #private;
	addClass: WorkRecorderFossil
getOrMakeCxxClassDescription in: #private;
	addClass: DirectEditionRecorderFossil
getOrMakeCxxClassDescription in: #private;
	addClass: DirectWorkRecorder
getOrMakeCxxClassDescription in: #private;
	addClass: IndirectWorkRecorderFossil
getOrMakeCxxClassDescription in: #private;
	addClass: DirectWorkRecorderFossil
getOrMakeCxxClassDescription in: #private;
	addClass: IndirectEditionRecorderFossil
getOrMakeCxxClassDescription in: #private;
	addClass: IndirectEditionRecorder
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:71907:
(CxxSystemOrganization fileNamed: 'ent')
	comment: ''!
*/
/*
udanax-top.st:71910:
(CxxSystemOrganization getOrMakeFileNamed: 'ent')
	addClass: Ent
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:71915:
(CxxSystemOrganization fileNamed: 'htree')
	comment: ''!
*/
/*
udanax-top.st:71918:
(CxxSystemOrganization getOrMakeFileNamed: 'htree')
	addClass: HistoryCrum
getOrMakeCxxClassDescription in: #public;
	addClass: HUpperCrum
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:71925:
(CxxSystemOrganization fileNamed: 'loaves')
	comment: ''!
*/
/*
udanax-top.st:71928:
(CxxSystemOrganization getOrMakeFileNamed: 'loaves')
	addClass: Loaf
getOrMakeCxxClassDescription in: #public;
	addClass: OExpandingLoaf
getOrMakeCxxClassDescription in: #public;
	addClass: InnerLoaf
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:71937:
(CxxSystemOrganization getOrMakeFileNamed: 'loaves')
	addClass: MergeBundlesStepper
getOrMakeCxxClassDescription in: #protected!
*/
/*
udanax-top.st:71942:
(CxxSystemOrganization getOrMakeFileNamed: 'loaves')
	addClass: DspLoaf
getOrMakeCxxClassDescription in: #private;
	addClass: SplitLoaf
getOrMakeCxxClassDescription in: #private;
	addClass: OVirtualLoaf
getOrMakeCxxClassDescription in: #private;
	addClass: SharedData
getOrMakeCxxClassDescription in: #private;
	addClass: RegionLoaf
getOrMakeCxxClassDescription in: #private;
	addClass: OPartialLoaf
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:71957:
(CxxSystemOrganization fileNamed: 'oroot')
	comment: ''!
*/
/*
udanax-top.st:71960:
(CxxSystemOrganization getOrMakeFileNamed: 'oroot')
	addClass: OPart
getOrMakeCxxClassDescription in: #public;
	addClass: OrglRoot
getOrMakeCxxClassDescription in: #public;
	addClass: ActualOrglRoot
getOrMakeCxxClassDescription in: #public;
	addClass: EmptyOrglRoot
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:71971:
(CxxSystemOrganization getOrMakeFileNamed: 'oroot')
	addClass: HBottomCrum
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:71976:
(CxxSystemOrganization fileNamed: 'branch')
	comment: ''!
*/
/*
udanax-top.st:71979:
(CxxSystemOrganization getOrMakeFileNamed: 'branch')
	hxxHeader: '' in: #public!
*/
/*
udanax-top.st:71982:
(CxxSystemOrganization getOrMakeFileNamed: 'branch')
	addClass: BranchDescription
getOrMakeCxxClassDescription in: #public;
	addClass: DagBranch
getOrMakeCxxClassDescription in: #public;
	addClass: TreeBranch
getOrMakeCxxClassDescription in: #public;
	addClass: RootBranch
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:71992:
(CxxSystemOrganization fileNamed: 'dagwood')
	comment: ''!
*/
/*
udanax-top.st:71995:
(CxxSystemOrganization getOrMakeFileNamed: 'dagwood')
	addClass: DagWood
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:72000:
(CxxSystemOrganization fileNamed: 'tracep')
	comment: ''!
*/
/*
udanax-top.st:72003:
(CxxSystemOrganization getOrMakeFileNamed: 'tracep')
	addClass: TracePosition
getOrMakeCxxClassDescription in: #public;
	addClass: BoundedTrace
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:72010:
(CxxSystemOrganization fileNamed: 'canopy')
	comment: ''!
*/
/*
udanax-top.st:72013:
(CxxSystemOrganization getOrMakeFileNamed: 'canopy')
	addClass: CanopyCrum
getOrMakeCxxClassDescription in: #public;
	addClass: SensorCrum
getOrMakeCxxClassDescription in: #public;
	addClass: BertCrum
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:72022:
(CxxSystemOrganization getOrMakeFileNamed: 'canopy')
	addClass: PropChanger
getOrMakeCxxClassDescription in: #protected;
	addClass: ActualPropChanger
getOrMakeCxxClassDescription in: #protected!
*/
/*
udanax-top.st:72029:
(CxxSystemOrganization getOrMakeFileNamed: 'canopy')
	addClass: HeightChanger
getOrMakeCxxClassDescription in: #private;
	addClass: Heaper2UInt32Cache
getOrMakeCxxClassDescription in: #private;
	addClass: CanopyCache
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:72038:
(CxxSystemOrganization fileNamed: 'props')
	comment: ''!
*/
/*
udanax-top.st:72041:
(CxxSystemOrganization getOrMakeFileNamed: 'props')
	addClass: PropFinder
getOrMakeCxxClassDescription in: #public;
	addClass: Prop
getOrMakeCxxClassDescription in: #public;
	addClass: BertProp
getOrMakeCxxClassDescription in: #public;
	addClass: SensorProp
getOrMakeCxxClassDescription in: #public;
	addClass: PropChange
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:72054:
(CxxSystemOrganization getOrMakeFileNamed: 'props')
	addClass: CannotPartializeChange
getOrMakeCxxClassDescription in: #private;
	addClass: DetectorWaitingChange
getOrMakeCxxClassDescription in: #private;
	addClass: FullPropChange
getOrMakeCxxClassDescription in: #private;
	addClass: SensorPropFinder
getOrMakeCxxClassDescription in: #private;
	addClass: OpenPropFinder
getOrMakeCxxClassDescription in: #private;
	addClass: BertPropChange
getOrMakeCxxClassDescription in: #private;
	addClass: EndorsementsChange
getOrMakeCxxClassDescription in: #private;
	addClass: AbstractRecorderFinder
getOrMakeCxxClassDescription in: #private;
	addClass: ClosedPropFinder
getOrMakeCxxClassDescription in: #private;
	addClass: PermissionsChange
getOrMakeCxxClassDescription in: #private;
	addClass: BertPropFinder
getOrMakeCxxClassDescription in: #private;
	addClass: SimpleRecorderFinder
getOrMakeCxxClassDescription in: #private;
	addClass: AnyRecorderFinder
getOrMakeCxxClassDescription in: #private;
	addClass: ContainedEditionRecorderEFinder
getOrMakeCxxClassDescription in: #private;
	addClass: ResultRecorderPFinder
getOrMakeCxxClassDescription in: #private;
	addClass: PartialityFinder
getOrMakeCxxClassDescription in: #private;
	addClass: BackfollowFinder
getOrMakeCxxClassDescription in: #private;
	addClass: AnyRecorderEFinder
getOrMakeCxxClassDescription in: #private;
	addClass: CannotPartializeFinder
getOrMakeCxxClassDescription in: #private;
	addClass: SensorPropChange
getOrMakeCxxClassDescription in: #private;
	addClass: SensorFinder
getOrMakeCxxClassDescription in: #private;
	addClass: CumulativeRecorderFinder
getOrMakeCxxClassDescription in: #private;
	addClass: BackfollowPFinder
getOrMakeCxxClassDescription in: #private;
	addClass: AnyRecorderPFinder
getOrMakeCxxClassDescription in: #private;
	addClass: OriginalResultRecorderEFinder
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:72107:
(CxxSystemOrganization fileNamed: 'counter')
	comment: ''!
*/
/*
udanax-top.st:72110:
(CxxSystemOrganization getOrMakeFileNamed: 'counter')
	addClass: Counter
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:72115:
(CxxSystemOrganization getOrMakeFileNamed: 'counter')
	addClass: BatchCounter
getOrMakeCxxClassDescription in: #private;
	addClass: SingleCounter
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:72122:
(CxxSystemOrganization fileNamed: 'grantab')
	comment: 'Presently the values called ''shift'' in this module are used with
divide and modulo operations rather than bit operations.  Thus
the minimum shift for a hashed key is 1 and not 0.'!
*/
/*
udanax-top.st:72127:
(CxxSystemOrganization getOrMakeFileNamed: 'grantab')
	hxxHeader: '' in: #public!
*/
/*
udanax-top.st:72130:
(CxxSystemOrganization getOrMakeFileNamed: 'grantab')
	cxxHeader: '#include <math.h>
' in: #public!
*/
/*
udanax-top.st:72133:
(CxxSystemOrganization getOrMakeFileNamed: 'grantab')
	addClass: GrandHashTable
getOrMakeCxxClassDescription in: #public;
	addClass: GrandHashSet
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:72139:
(CxxSystemOrganization getOrMakeFileNamed: 'grantab')
	hxxHeader: '#include "fhashx.hxx"' in: #private!
*/
/*
udanax-top.st:72142:
(CxxSystemOrganization getOrMakeFileNamed: 'grantab')
	cxxHeader: '#include "fhashx.hxx"
' in: #private!
*/
/*
udanax-top.st:72145:
(CxxSystemOrganization getOrMakeFileNamed: 'grantab')
	addClass: GrandHashSetStepper
getOrMakeCxxClassDescription in: #private;
	addClass: GrandOverflowStepper
getOrMakeCxxClassDescription in: #private;
	addClass: GrandNodeReinserter
getOrMakeCxxClassDescription in: #private;
	addClass: GrandHashTableStepper
getOrMakeCxxClassDescription in: #private;
	addClass: GrandNodeDoubler
getOrMakeCxxClassDescription in: #private;
	addClass: GrandEntry
getOrMakeCxxClassDescription in: #private;
	addClass: GrandTableEntry
getOrMakeCxxClassDescription in: #private;
	addClass: GrandDataPage
getOrMakeCxxClassDescription in: #private;
	addClass: GrandDataPageStepper
getOrMakeCxxClassDescription in: #private;
	addClass: GrandOverflow
getOrMakeCxxClassDescription in: #private;
	addClass: GrandSetEntry
getOrMakeCxxClassDescription in: #private;
	addClass: GrandNodeStepper
getOrMakeCxxClassDescription in: #private;
	addClass: GrandNode
getOrMakeCxxClassDescription in: #private;
	addClass: ExponentialHashMap
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:72175:
(CxxSystemOrganization getOrMakeFileNamed: 'grantab')
	hxxHeader: '#include <stream.h>' in: #test!
*/
/*
udanax-top.st:72178:
(CxxSystemOrganization getOrMakeFileNamed: 'grantab')
	addClass: GrandHashTableTester
getOrMakeCxxClassDescription in: #test;
	addClass: GrandHashSetTester
getOrMakeCxxClassDescription in: #test!
*/
/*
udanax-top.st:72184:
(CxxSystemOrganization fileNamed: 'worksrv')
	comment: ''!
*/
/*
udanax-top.st:72187:
(CxxSystemOrganization getOrMakeFileNamed: 'worksrv')
	hxxHeader: '#define NOACK void' in: #public!
*/
/*
udanax-top.st:72190:
(CxxSystemOrganization getOrMakeFileNamed: 'worksrv')
	addClass: WorksBootMaker
getOrMakeCxxClassDescription in: #public;
	addClass: WorksIniter
getOrMakeCxxClassDescription in: #public;
	addClass: FeWorksBootMaker
getOrMakeCxxClassDescription in: #public;
	addClass: WorksWaitDetector
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:72200:
(CxxSystemOrganization fileNamed: 'become')
	comment: 'This module exists purely to test "become" support.  
The implememtation of "become" support is elsewhere (in tofu & init).'!
*/
/*
udanax-top.st:72204:
(CxxSystemOrganization getOrMakeFileNamed: 'become')
	addClass: BecomeTester
getOrMakeCxxClassDescription in: #test;
	addClass: Chameleon
getOrMakeCxxClassDescription in: #test;
	addClass: Butterfly
getOrMakeCxxClassDescription in: #test;
	addClass: DeadMoth
getOrMakeCxxClassDescription in: #test;
	addClass: Moth
getOrMakeCxxClassDescription in: #test;
	addClass: IronButterfly
getOrMakeCxxClassDescription in: #test;
	addClass: GoldButterfly
getOrMakeCxxClassDescription in: #test;
	addClass: LeadButterfly
getOrMakeCxxClassDescription in: #test;
	addClass: DeadButterfly
getOrMakeCxxClassDescription in: #test!
*/
/*
udanax-top.st:72225:
(CxxSystemOrganization fileNamed: 'cache')
	comment: ''!
*/
/*
udanax-top.st:72228:
(CxxSystemOrganization getOrMakeFileNamed: 'cache')
	addClass: InstanceCache
getOrMakeCxxClassDescription in: #public;
	addClass: CacheManager
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:72235:
(CxxSystemOrganization getOrMakeFileNamed: 'cache')
	addClass: SuspendedHeaper
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:72240:
(CxxSystemOrganization fileNamed: 'gchooks')
	comment: ''!
*/
/*
udanax-top.st:72243:
(CxxSystemOrganization getOrMakeFileNamed: 'gchooks')
	cxxHeader: '#include <osfcn.h>
#include <stdlib.h>' in: #public!
*/
/*
udanax-top.st:72247:
(CxxSystemOrganization getOrMakeFileNamed: 'gchooks')
	addClass: StackExaminer
getOrMakeCxxClassDescription in: #public;
	addClass: SanitationEngineer
getOrMakeCxxClassDescription in: #public;
	addClass: DeleteExecutor
getOrMakeCxxClassDescription in: #public;
	addClass: RepairEngineer
getOrMakeCxxClassDescription in: #public;
	addClass: CloseExecutor
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:72259:
(CxxSystemOrganization fileNamed: 'hlogger')
	comment: ''!
*/
/*
udanax-top.st:72262:
(CxxSystemOrganization getOrMakeFileNamed: 'hlogger')
	addClass: SwitchLogger
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:72267:
(CxxSystemOrganization getOrMakeFileNamed: 'hlogger')
	addClass: LogTester
getOrMakeCxxClassDescription in: #test!
*/
/*
udanax-top.st:72272:
(CxxSystemOrganization fileNamed: 'tokens')
	comment: ''!
*/
/*
udanax-top.st:72275:
(CxxSystemOrganization getOrMakeFileNamed: 'tokens')
	addClass: TokenSource
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:72280:
(CxxSystemOrganization fileNamed: 'primtab')
	comment: ''!
*/
/*
udanax-top.st:72283:
(CxxSystemOrganization getOrMakeFileNamed: 'primtab')
	cxxHeader: '#include "fhashx.hxx"' in: #public!
*/
/*
udanax-top.st:72286:
(CxxSystemOrganization getOrMakeFileNamed: 'primtab')
	addClass: PrimIndexTable
getOrMakeCxxClassDescription in: #public;
	addClass: PrimIndexTableStepper
getOrMakeCxxClassDescription in: #public;
	addClass: PrimPtr2PtrTable
getOrMakeCxxClassDescription in: #public;
	addClass: PrimSet
getOrMakeCxxClassDescription in: #public;
	addClass: PrimPtrTableStepper
getOrMakeCxxClassDescription in: #public;
	addClass: PrimPtr2PtrTableStepper
getOrMakeCxxClassDescription in: #public;
	addClass: PrimPtrTable
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:72302:
(CxxSystemOrganization getOrMakeFileNamed: 'primtab')
	addClass: PrimRemovedObject
getOrMakeCxxClassDescription in: #private;
	addClass: PrimPtrTableExecutor
getOrMakeCxxClassDescription in: #private;
	addClass: PrimSetExecutor
getOrMakeCxxClassDescription in: #private;
	addClass: PrimSetStepper
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:72313:
(CxxSystemOrganization getOrMakeFileNamed: 'primtab')
	addClass: PrimPtrTableTester
getOrMakeCxxClassDescription in: #test;
	addClass: PrimIndexTableTester
getOrMakeCxxClassDescription in: #test!
*/
/*
udanax-top.st:72320:
(CxxSystemOrganization fileNamed: 'primval')
	comment: ''!
*/
/*
udanax-top.st:72323:
(CxxSystemOrganization getOrMakeFileNamed: 'primval')
	cxxHeader: '#include <math.h>
' in: #public!
*/
/*
udanax-top.st:72327:
(CxxSystemOrganization getOrMakeFileNamed: 'primval')
	addClass: PrimSpec
getOrMakeCxxClassDescription in: #public;
	addClass: PrimIntegerSpec
getOrMakeCxxClassDescription in: #public;
	addClass: PrimFloatSpec
getOrMakeCxxClassDescription in: #public;
	addClass: PrimValue
getOrMakeCxxClassDescription in: #public;
	addClass: PrimFloatValue
getOrMakeCxxClassDescription in: #public;
	addClass: PrimIEEE64
getOrMakeCxxClassDescription in: #public;
	addClass: PrimIntValue
getOrMakeCxxClassDescription in: #public;
	addClass: PrimPointerSpec
getOrMakeCxxClassDescription in: #public;
	addClass: PrimIEEE32
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:72347:
(CxxSystemOrganization fileNamed: 'rcmain')
	comment: ''!
*/
/*
udanax-top.st:72350:
(CxxSystemOrganization getOrMakeFileNamed: 'rcmain')
	addClass: MainDummy
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:72355:
(CxxSystemOrganization fileNamed: 'set')
	comment: 'This file group has no comment'!
*/
/*
udanax-top.st:72358:
(CxxSystemOrganization getOrMakeFileNamed: 'set')
	hxxHeader: '/- xpp class -/
' in: #public!
*/
/*
udanax-top.st:72362:
(CxxSystemOrganization getOrMakeFileNamed: 'set')
	comment: 'This group of files represents the Set type of collections for X++.  There are 3
basic categories:
	 ScruSet - which is read-only but is not guaranteed to not change,
	 ImmuSet - a set which is guaranteed not to change once constructed, and
	 MuSet - a set which has a protocol to allow it to be changed.
'in: #public!
*/
/*
udanax-top.st:72369:
(CxxSystemOrganization getOrMakeFileNamed: 'set')
	addClass: ScruSet
getOrMakeCxxClassDescription in: #public;
	addClass: UnionRecruiter
getOrMakeCxxClassDescription in: #public;
	addClass: ImmuSet
getOrMakeCxxClassDescription in: #public;
	addClass: MuSet
getOrMakeCxxClassDescription in: #public;
	addClass: SetAccumulator
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:72381:
(CxxSystemOrganization getOrMakeFileNamed: 'set')
	addClass: ImmuSetOnMu
getOrMakeCxxClassDescription in: #private;
	addClass: HashSet
getOrMakeCxxClassDescription in: #private;
	addClass: EmptyImmuSet
getOrMakeCxxClassDescription in: #private;
	addClass: ActualHashSet
getOrMakeCxxClassDescription in: #private;
	addClass: TinyImmuSet
getOrMakeCxxClassDescription in: #private;
	addClass: HashSetStepper
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:72396:
(CxxSystemOrganization getOrMakeFileNamed: 'set')
	hxxHeader: '
#include <stream.h>' in: #test!
*/
/*
udanax-top.st:72400:
(CxxSystemOrganization getOrMakeFileNamed: 'set')
	cxxHeader: '#include "setp.hxx"
#include "tablesx.hxx"
#include "choosex.hxx"' in: #test!
*/
/*
udanax-top.st:72404:
(CxxSystemOrganization getOrMakeFileNamed: 'set')
	addClass: ScruSetTester
getOrMakeCxxClassDescription in: #test;
	addClass: SetTester
getOrMakeCxxClassDescription in: #test;
	addClass: MuSetTester
getOrMakeCxxClassDescription in: #test;
	addClass: ImmuSetTester
getOrMakeCxxClassDescription in: #test;
	addClass: SHTO
getOrMakeCxxClassDescription in: #test;
	addClass: HashSetTester
getOrMakeCxxClassDescription in: #test!
*/
/*
udanax-top.st:72418:
(CxxSystemOrganization fileNamed: 'settab')
	comment: ''!
*/
/*
udanax-top.st:72421:
(CxxSystemOrganization getOrMakeFileNamed: 'settab')
	addClass: SetTable
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:72426:
(CxxSystemOrganization getOrMakeFileNamed: 'settab')
	addClass: SetTableStepper
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:72431:
(CxxSystemOrganization getOrMakeFileNamed: 'settab')
	addClass: SetTableTester
getOrMakeCxxClassDescription in: #test!
*/
/*
udanax-top.st:72436:
(CxxSystemOrganization fileNamed: 'cross')
	comment: ''!
*/
/*
udanax-top.st:72439:
(CxxSystemOrganization getOrMakeFileNamed: 'cross')
	hxxHeader: '/- xpp class -/
' in: #public!
*/
/*
udanax-top.st:72443:
(CxxSystemOrganization getOrMakeFileNamed: 'cross')
	addClass: CrossSpace
getOrMakeCxxClassDescription in: #public;
	addClass: CrossRegion
getOrMakeCxxClassDescription in: #public;
	addClass: CrossOrderSpec
getOrMakeCxxClassDescription in: #public;
	addClass: CrossMapping
getOrMakeCxxClassDescription in: #public;
	addClass: Tuple
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:72455:
(CxxSystemOrganization getOrMakeFileNamed: 'cross')
	addClass: ActualTuple
getOrMakeCxxClassDescription in: #private;
	addClass: GenericCrossSimpleRegionStepper
getOrMakeCxxClassDescription in: #private;
	addClass: GenericCrossRegion
getOrMakeCxxClassDescription in: #private;
	addClass: GenericCrossSpace
getOrMakeCxxClassDescription in: #private;
	addClass: BoxAccumulator
getOrMakeCxxClassDescription in: #private;
	addClass: BoxStepper
getOrMakeCxxClassDescription in: #private;
	addClass: TupleStepper
getOrMakeCxxClassDescription in: #private;
	addClass: BoxProjectionStepper
getOrMakeCxxClassDescription in: #private;
	addClass: GenericCrossDsp
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:72476:
(CxxSystemOrganization getOrMakeFileNamed: 'cross')
	addClass: CrossTester
getOrMakeCxxClassDescription in: #test!
*/
/*
udanax-top.st:72481:
(CxxSystemOrganization fileNamed: 'edge')
	comment: ''!
*/
/*
udanax-top.st:72484:
(CxxSystemOrganization getOrMakeFileNamed: 'edge')
	addClass: EdgeManager
getOrMakeCxxClassDescription in: #protected;
	addClass: TransitionEdge
getOrMakeCxxClassDescription in: #protected!
*/
/*
udanax-top.st:72491:
(CxxSystemOrganization getOrMakeFileNamed: 'edge')
	addClass: EdgeSimpleRegionStepper
getOrMakeCxxClassDescription in: #private;
	addClass: EdgeStepper
getOrMakeCxxClassDescription in: #private;
	addClass: EdgeAccumulator
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:72500:
(CxxSystemOrganization fileNamed: 'filter')
	comment: ''!
*/
/*
udanax-top.st:72503:
(CxxSystemOrganization getOrMakeFileNamed: 'filter')
	hxxHeader: '/- xpp class -/' in: #public!
*/
/*
udanax-top.st:72506:
(CxxSystemOrganization getOrMakeFileNamed: 'filter')
	addClass: Filter
getOrMakeCxxClassDescription in: #public;
	addClass: Joint
getOrMakeCxxClassDescription in: #public;
	addClass: FilterPosition
getOrMakeCxxClassDescription in: #public;
	addClass: FilterSpace
getOrMakeCxxClassDescription in: #public;
	addClass: RegionDelta
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:72518:
(CxxSystemOrganization getOrMakeFileNamed: 'filter')
	addClass: AndFilter
getOrMakeCxxClassDescription in: #private;
	addClass: SubsetFilter
getOrMakeCxxClassDescription in: #private;
	addClass: OpenFilter
getOrMakeCxxClassDescription in: #private;
	addClass: OrFilter
getOrMakeCxxClassDescription in: #private;
	addClass: FilterDsp
getOrMakeCxxClassDescription in: #private;
	addClass: NotSupersetFilter
getOrMakeCxxClassDescription in: #private;
	addClass: SupersetFilter
getOrMakeCxxClassDescription in: #private;
	addClass: ClosedFilter
getOrMakeCxxClassDescription in: #private;
	addClass: NotSubsetFilter
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:72539:
(CxxSystemOrganization getOrMakeFileNamed: 'filter')
	addClass: FilterTester
getOrMakeCxxClassDescription in: #test!
*/
/*
udanax-top.st:72544:
(CxxSystemOrganization fileNamed: 'hspace')
	comment: ''!
*/
/*
udanax-top.st:72547:
(CxxSystemOrganization getOrMakeFileNamed: 'hspace')
	addClass: UnOrdered
getOrMakeCxxClassDescription in: #public;
	addClass: HeaperSpace
getOrMakeCxxClassDescription in: #public;
	addClass: HeaperAsPosition
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:72556:
(CxxSystemOrganization getOrMakeFileNamed: 'hspace')
	cxxHeader: '#include "choosex.hxx"' in: #private!
*/
/*
udanax-top.st:72559:
(CxxSystemOrganization getOrMakeFileNamed: 'hspace')
	addClass: HeaperDsp
getOrMakeCxxClassDescription in: #private;
	addClass: SetRegion
getOrMakeCxxClassDescription in: #private;
	addClass: HeaperRegion
getOrMakeCxxClassDescription in: #private;
	addClass: StrongAsPosition
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:72569:
(CxxSystemOrganization fileNamed: 'integer')
	comment: ''!
*/
/*
udanax-top.st:72572:
(CxxSystemOrganization getOrMakeFileNamed: 'integer')
	hxxHeader: '#define Integer0 IntegerPos::make(0)
' in: #public!
*/
/*
udanax-top.st:72576:
(CxxSystemOrganization getOrMakeFileNamed: 'integer')
	addClass: IntegerRegion
getOrMakeCxxClassDescription in: #public;
	addClass: IntegerSpace
getOrMakeCxxClassDescription in: #public;
	addClass: IntegerMapping
getOrMakeCxxClassDescription in: #public;
	addClass: IntegerPos
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:72586:
(CxxSystemOrganization getOrMakeFileNamed: 'integer')
	hxxHeader: '' in: #private!
*/
/*
udanax-top.st:72589:
(CxxSystemOrganization getOrMakeFileNamed: 'integer')
	addClass: IntegerArrangement
getOrMakeCxxClassDescription in: #private;
	addClass: DescendingIntegerStepper
getOrMakeCxxClassDescription in: #private;
	addClass: IntegerUpOrder
getOrMakeCxxClassDescription in: #private;
	addClass: IntegerEdgeStepper
getOrMakeCxxClassDescription in: #private;
	addClass: IntegerEdgeAccumulator
getOrMakeCxxClassDescription in: #private;
	addClass: IntegerSimpleRegionStepper
getOrMakeCxxClassDescription in: #private;
	addClass: AscendingIntegerStepper
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:72605:
(CxxSystemOrganization getOrMakeFileNamed: 'integer')
	addClass: IntegerRegionTester
getOrMakeCxxClassDescription in: #test!
*/
/*
udanax-top.st:72610:
(CxxSystemOrganization fileNamed: 'real')
	comment: ''!
*/
/*
udanax-top.st:72613:
(CxxSystemOrganization getOrMakeFileNamed: 'real')
	hxxHeader: '#define IEEE8 Int8
' in: #public!
*/
/*
udanax-top.st:72617:
(CxxSystemOrganization getOrMakeFileNamed: 'real')
	addClass: RealRegion
getOrMakeCxxClassDescription in: #public;
	addClass: RealSpace
getOrMakeCxxClassDescription in: #public;
	addClass: RealPos
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:72625:
(CxxSystemOrganization getOrMakeFileNamed: 'real')
	addClass: RealDsp
getOrMakeCxxClassDescription in: #private;
	addClass: RealUpOrder
getOrMakeCxxClassDescription in: #private;
	addClass: RealManager
getOrMakeCxxClassDescription in: #private;
	addClass: RealEdge
getOrMakeCxxClassDescription in: #private;
	addClass: RealStepper
getOrMakeCxxClassDescription in: #private;
	addClass: IEEE32Pos
getOrMakeCxxClassDescription in: #private;
	addClass: BeforeReal
getOrMakeCxxClassDescription in: #private;
	addClass: IEEE64Pos
getOrMakeCxxClassDescription in: #private;
	addClass: AfterReal
getOrMakeCxxClassDescription in: #private;
	addClass: IEEE8Pos
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:72648:
(CxxSystemOrganization getOrMakeFileNamed: 'real')
	addClass: RealTester
getOrMakeCxxClassDescription in: #test!
*/
/*
udanax-top.st:72653:
(CxxSystemOrganization fileNamed: 'sequenc')
	comment: ''!
*/
/*
udanax-top.st:72656:
(CxxSystemOrganization getOrMakeFileNamed: 'sequenc')
	cxxHeader: '#include <sys/types.h>
#ifndef WIN32
#	include <sys/time.h>
#else
#	include <sys/timeb.h>
#endif /- WIN32 -/
' in: #public!
*/
/*
udanax-top.st:72665:
(CxxSystemOrganization getOrMakeFileNamed: 'sequenc')
	addClass: SequenceMapping
getOrMakeCxxClassDescription in: #public;
	addClass: SequenceRegion
getOrMakeCxxClassDescription in: #public;
	addClass: Sequence
getOrMakeCxxClassDescription in: #public;
	addClass: SequenceSpace
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:72675:
(CxxSystemOrganization getOrMakeFileNamed: 'sequenc')
	addClass: SequenceEdge
getOrMakeCxxClassDescription in: #private;
	addClass: SequenceManager
getOrMakeCxxClassDescription in: #private;
	addClass: BeforeSequencePrefix
getOrMakeCxxClassDescription in: #private;
	addClass: SequenceStepper
getOrMakeCxxClassDescription in: #private;
	addClass: SequenceUpOrder
getOrMakeCxxClassDescription in: #private;
	addClass: AfterSequence
getOrMakeCxxClassDescription in: #private;
	addClass: BeforeSequence
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:72692:
(CxxSystemOrganization getOrMakeFileNamed: 'sequenc')
	addClass: SequenceTester
getOrMakeCxxClassDescription in: #test!
*/
/*
udanax-top.st:72697:
(CxxSystemOrganization fileNamed: 'space')
	comment: ''!
*/
/*
udanax-top.st:72700:
(CxxSystemOrganization getOrMakeFileNamed: 'space')
	hxxHeader: '/- xpp class -/
typedef enum { LESS_THAN, EQUAL, GREATER_THAN, INCOMPARABLE }
	OrderEnum;' in: #public!
*/
/*
udanax-top.st:72706:
(CxxSystemOrganization getOrMakeFileNamed: 'space')
	addClass: Mapping
getOrMakeCxxClassDescription in: #public;
	addClass: Arrangement
getOrMakeCxxClassDescription in: #public;
	addClass: CoordinateSpace
getOrMakeCxxClassDescription in: #public;
	addClass: OrderSpec
getOrMakeCxxClassDescription in: #public;
	addClass: XnRegion
getOrMakeCxxClassDescription in: #public;
	addClass: Dsp
getOrMakeCxxClassDescription in: #public;
	addClass: Position
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:72722:
(CxxSystemOrganization getOrMakeFileNamed: 'space')
	addClass: MergeStepper
getOrMakeCxxClassDescription in: #protected;
	addClass: ExplicitArrangement
getOrMakeCxxClassDescription in: #protected;
	addClass: IdentityDsp
getOrMakeCxxClassDescription in: #protected;
	addClass: BasicSpace
getOrMakeCxxClassDescription in: #protected!
*/
/*
udanax-top.st:72733:
(CxxSystemOrganization getOrMakeFileNamed: 'space')
	addClass: DisjointRegionStepper
getOrMakeCxxClassDescription in: #private;
	addClass: ConstantMapping
getOrMakeCxxClassDescription in: #private;
	addClass: SimpleMapping
getOrMakeCxxClassDescription in: #private;
	addClass: ReverseOrder
getOrMakeCxxClassDescription in: #private;
	addClass: EmptyMapping
getOrMakeCxxClassDescription in: #private;
	addClass: CompositeMapping
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:72748:
(CxxSystemOrganization getOrMakeFileNamed: 'space')
	addClass: RegionTester
getOrMakeCxxClassDescription in: #test!
*/
/*
udanax-top.st:72753:
(CxxSystemOrganization fileNamed: 'stepper')
	comment: ''!
*/
/*
udanax-top.st:72756:
(CxxSystemOrganization getOrMakeFileNamed: 'stepper')
	hxxHeader: '#ifdef MANUAL_CRUTCH
#	define BEGIN_FOR_EACH(TYPE,VAR,EXPR) {						\
		SPTR(Stepper) OF(TYPE) loop_Stepper = (EXPR);			\
		SPTR(TYPE) VAR;												\
		while ((VAR = CAST(TYPE,loop_Stepper->fetch())) != NULL ) {
#define END_FOR_EACH											\
			loop_Stepper->step();									\
		}															\
		loop_Stepper->destroy();									\
	}
#else /- MANUAL_CRUTCH -/
#	define BEGIN_FOR_EACH(TYPE,VAR,EXPR) {					\
		SPTR(TYPE) VAR;											\
		for(SPTR(Stepper) OF(TYPE) loop_Stepper = (EXPR);		\
				loop_Stepper->hasValue();							\
				loop_Stepper->step()) {							\
			VAR = CAST(TYPE,loop_Stepper->fetch());	
#define END_FOR_EACH											\
		}															\
		loop_Stepper->destroy();									\
	}
#endif /- MANUAL_CRUTCH -/
#ifdef MANUAL_CRUTCH
#	define BEGIN_FOR_POSITIONS(TYPE1,POSITION,TYPE2,VALUE,EXPR) {						\
		SPTR(TableStepper) OF(TYPE1) loop_Stepper = CAST(TableStepper,(EXPR));		\
		SPTR(TYPE1) POSITION;											\
		SPTR(TYPE2) VALUE;										\
		while ( loop_Stepper->hasValue() ) {						\
			POSITION = CAST(TYPE1,loop_Stepper->position());				\
			VALUE = CAST(TYPE2,loop_Stepper->fetch());	
			
#define END_FOR_POSITIONS				\
			loop_Stepper->step();				\
		}										\
		loop_Stepper->destroy();				\
	}
#	define BEGIN_FOR_INDICES(INDEX,TYPE2,VALUE,EXPR) {						\
		SPTR(TableStepper) OF(TYPE1) loop_Stepper = CAST(TableStepper,(EXPR));		\
		IntegerVar INDEX;											\
		SPTR(TYPE2) VALUE;										\
		while ( loop_Stepper->hasValue() ) {						\
			INDEX = loop_Stepper->index();				\
			VALUE = CAST(TYPE2,loop_Stepper->fetch());	
			
#define END_FOR_INDICES				\
			loop_Stepper->step();				\
		}										\
		loop_Stepper->destroy();				\
	}
#else /- MANUAL_CRUTCH -/
#	define BEGIN_FOR_POSITIONS(TYPE1,POSITION,TYPE2,VALUE,EXPR) {			\
		SPTR(TYPE1) POSITION;											\
		SPTR(TYPE2) VALUE;										\
		for(	 SPTR(TableStepper) OF(TYPE1) loop_Stepper = CAST(TableStepper,(EXPR));		\
				loop_Stepper->hasValue();							\
				loop_Stepper->step()) {							\
			POSITION = CAST(TYPE1,loop_Stepper->position());				\
			VALUE = CAST(TYPE2,loop_Stepper->fetch());
#define END_FOR_POSITIONS				\
		}										\
		loop_Stepper->destroy();				\
	}
#	define BEGIN_FOR_INDICES(INDEX,TYPE2,VALUE,EXPR) {			\
		IntegerVar INDEX;											\
		SPTR(TYPE2) VALUE;										\
		for(	 SPTR(TableStepper) OF(TYPE1) loop_Stepper = CAST(TableStepper,(EXPR));		\
				loop_Stepper->hasValue();							\
				loop_Stepper->step()) {							\
			INDEX = loop_Stepper->index();				\
			VALUE = CAST(TYPE2,loop_Stepper->fetch());
#define END_FOR_INDICES				\
		}										\
		loop_Stepper->destroy();				\
	}
#endif /- MANUAL_CRUTCH -/
' in: #public!
*/
/*
udanax-top.st:72848:
(CxxSystemOrganization getOrMakeFileNamed: 'stepper')
	addClass: Stepper
getOrMakeCxxClassDescription in: #public;
	addClass: Accumulator
getOrMakeCxxClassDescription in: #public;
	addClass: TableStepper
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:72856:
(CxxSystemOrganization getOrMakeFileNamed: 'stepper')
	addClass: EmptyStepper
getOrMakeCxxClassDescription in: #private;
	addClass: PtrArrayStepper
getOrMakeCxxClassDescription in: #private;
	addClass: PtrArrayAccumulator
getOrMakeCxxClassDescription in: #private;
	addClass: ItemStepper
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:72867:
(CxxSystemOrganization fileNamed: 'array')
	comment: ''!
*/
/*
udanax-top.st:72870:
(CxxSystemOrganization getOrMakeFileNamed: 'array')
	hxxHeader: '/- xpp class -/
' in: #public!
*/
/*
udanax-top.st:72874:
(CxxSystemOrganization getOrMakeFileNamed: 'array')
	cxxHeader: '#include <string.h>
#include <stream.h>
' in: #public!
*/
/*
udanax-top.st:72878:
(CxxSystemOrganization getOrMakeFileNamed: 'array')
	addClass: MuArray
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:72882:
(CxxSystemOrganization getOrMakeFileNamed: 'array')
	hxxHeader: '' in: #private!
*/
/*
udanax-top.st:72885:
(CxxSystemOrganization getOrMakeFileNamed: 'array')
	addClass: OffsetScruArray
getOrMakeCxxClassDescription in: #private;
	addClass: ArrayStepper
getOrMakeCxxClassDescription in: #private;
	addClass: AscendingArrayStepper
getOrMakeCxxClassDescription in: #private;
	addClass: ArrayAccumulator
getOrMakeCxxClassDescription in: #private;
	addClass: OffsetArrayStepper
getOrMakeCxxClassDescription in: #private;
	addClass: ActualArray
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:72899:
(CxxSystemOrganization fileNamed: 'hashtab')
	comment: ''!
*/
/*
udanax-top.st:72902:
(CxxSystemOrganization getOrMakeFileNamed: 'hashtab')
	hxxHeader: '' in: #public!
*/
/*
udanax-top.st:72905:
(CxxSystemOrganization getOrMakeFileNamed: 'hashtab')
	cxxHeader: '#include "integerx.hxx"' in: #public!
*/
/*
udanax-top.st:72907:
(CxxSystemOrganization getOrMakeFileNamed: 'hashtab')
	addClass: HashTable
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:72911:
(CxxSystemOrganization getOrMakeFileNamed: 'hashtab')
	addClass: ActualHashTable
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:72916:
(CxxSystemOrganization getOrMakeFileNamed: 'hashtab')
	hxxHeader: '
#include <stream.h>' in: #test!
*/
/*
udanax-top.st:72920:
(CxxSystemOrganization getOrMakeFileNamed: 'hashtab')
	cxxHeader: '#include "integerx.hxx" /- missed by include scanner -/' in: #test!
*/
/*
udanax-top.st:72922:
(CxxSystemOrganization getOrMakeFileNamed: 'hashtab')
	addClass: HashTableTester
getOrMakeCxxClassDescription in: #test!
*/
/*
udanax-top.st:72926:
(CxxSystemOrganization fileNamed: 'inttab')
	comment: ''!
*/
/*
udanax-top.st:72929:
(CxxSystemOrganization getOrMakeFileNamed: 'inttab')
	comment: 'This file represents the tables with Integer domains.  It provides the abstract
classes IntegerTable and its (abstract) subclass Array.  IntegerTable may be used for
any Integer keys, while Array is restricted to be zero based, and to have contiguous
Integer keys.'in: #public!
*/
/*
udanax-top.st:72935:
(CxxSystemOrganization getOrMakeFileNamed: 'inttab')
	addClass: IntegerTable
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:72939:
(CxxSystemOrganization getOrMakeFileNamed: 'inttab')
	addClass: IntegerTableStepper
getOrMakeCxxClassDescription in: #protected!
*/
/*
udanax-top.st:72944:
(CxxSystemOrganization getOrMakeFileNamed: 'inttab')
	hxxHeader: '' in: #private!
*/
/*
udanax-top.st:72947:
(CxxSystemOrganization getOrMakeFileNamed: 'inttab')
	addClass: ITAscendingStepper
getOrMakeCxxClassDescription in: #private;
	addClass: OberIntegerTable
getOrMakeCxxClassDescription in: #private;
	addClass: ActualIntegerTable
getOrMakeCxxClassDescription in: #private;
	addClass: COWIntegerTable
getOrMakeCxxClassDescription in: #private;
	addClass: ITDescendingStepper
getOrMakeCxxClassDescription in: #private;
	addClass: ITGenericStepper
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:72961:
(CxxSystemOrganization getOrMakeFileNamed: 'inttab')
	hxxHeader: '#include <stream.h>' in: #test!
*/
/*
udanax-top.st:72964:
(CxxSystemOrganization getOrMakeFileNamed: 'inttab')
	addClass: IntegerTableTester
getOrMakeCxxClassDescription in: #test!
*/
/*
udanax-top.st:72968:
(CxxSystemOrganization fileNamed: 'tabent')
	comment: ''!
*/
/*
udanax-top.st:72971:
(CxxSystemOrganization getOrMakeFileNamed: 'tabent')
	addClass: TableEntry
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:72976:
(CxxSystemOrganization getOrMakeFileNamed: 'tabent')
	addClass: IndexEntry
getOrMakeCxxClassDescription in: #private;
	addClass: PositionEntry
getOrMakeCxxClassDescription in: #private;
	addClass: BucketArrayStepper
getOrMakeCxxClassDescription in: #private;
	addClass: HeaperAsEntry
getOrMakeCxxClassDescription in: #private;
	addClass: HashIndexEntry
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:72989:
(CxxSystemOrganization getOrMakeFileNamed: 'tabent')
	cxxHeader: '#include "stepperx.hxx"' in: #test!
*/
/*
udanax-top.st:72992:
(CxxSystemOrganization getOrMakeFileNamed: 'tabent')
	addClass: TableEntryTester
getOrMakeCxxClassDescription in: #test!
*/
/*
udanax-top.st:72996:
(CxxSystemOrganization fileNamed: 'tables')
	comment: ''!
*/
/*
udanax-top.st:72999:
(CxxSystemOrganization getOrMakeFileNamed: 'tables')
	comment: 'This file declares the Table classes.  These are the basic X++ collection
classes.  They are designed to be purely collections, with no excess
protocol for ordering or enumeration.  A table is considered to be a collection
which maps from Positions to arbitrary objects (Heapers).  They are
defined as having a coordinate space for the domain.
mumble mumble, more explanation later.'in: #public!
*/
/*
udanax-top.st:73008:
(CxxSystemOrganization getOrMakeFileNamed: 'tables')
	addClass: ScruTable
getOrMakeCxxClassDescription in: #public;
	addClass: MuTable
getOrMakeCxxClassDescription in: #public;
	addClass: ImmuTable
getOrMakeCxxClassDescription in: #public;
	addClass: Pair
getOrMakeCxxClassDescription in: #public;
	addClass: TableAccumulator
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:73020:
(CxxSystemOrganization getOrMakeFileNamed: 'tables')
	cxxHeader: '#include "choosex.hxx"
' in: #private!
*/
/*
udanax-top.st:73024:
(CxxSystemOrganization getOrMakeFileNamed: 'tables')
	addClass: OffsetImmuTable
getOrMakeCxxClassDescription in: #private;
	addClass: OffsetScruTableStepper
getOrMakeCxxClassDescription in: #private;
	addClass: IntegerScruTable
getOrMakeCxxClassDescription in: #private;
	addClass: ImmuTableOnMu
getOrMakeCxxClassDescription in: #private;
	addClass: OffsetScruTable
getOrMakeCxxClassDescription in: #private!
*/
/*
udanax-top.st:73036:
(CxxSystemOrganization fileNamed: 'tabtool')
	comment: ''!
*/
/*
udanax-top.st:73039:
(CxxSystemOrganization getOrMakeFileNamed: 'tabtool')
	addClass: PrimeSizeProvider
getOrMakeCxxClassDescription in: #public;
	addClass: LPPrimeSizeProvider
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:73046:
(CxxSystemOrganization fileNamed: 'tester')
	comment: ''!
*/
/*
udanax-top.st:73049:
(CxxSystemOrganization getOrMakeFileNamed: 'tester')
	hxxHeader: '#include <string.h>' in: #public!
*/
/*
udanax-top.st:73052:
(CxxSystemOrganization getOrMakeFileNamed: 'tester')
	addClass: Tester
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:73056:
(CxxSystemOrganization getOrMakeFileNamed: 'tester')
	hxxHeader: '#include <string.h>
#include <stream.h>' in: #test!
*/
/*
udanax-top.st:73060:
(CxxSystemOrganization getOrMakeFileNamed: 'tester')
	addClass: HelloTester
getOrMakeCxxClassDescription in: #test!
*/
/*
udanax-top.st:73064:
(CxxSystemOrganization fileNamed: 'thunk')
	comment: ''!
*/
/*
udanax-top.st:73067:
(CxxSystemOrganization getOrMakeFileNamed: 'thunk')
	addClass: Thunk
getOrMakeCxxClassDescription in: #public;
	addClass: EchoThunk
getOrMakeCxxClassDescription in: #public;
	addClass: CommentThunk
getOrMakeCxxClassDescription in: #public!
*/
/*
udanax-top.st:73076:
CxxSystemOrganization tree: (CxxSystemOrganization tree addChild:
	((CxxTreeAssociation key: 'top' value: nil)
 addChild: ((CxxTreeAssociation key: #xhs value: nil)
 addChild: ((CxxTreeAssociation key: #comm value: #dir)
 addChild: ((CxxTreeAssociation key: #bootpln value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #comdtct value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #handlrs value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #memstr value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #negoti8 value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #nscotty value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #portal value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #prolstn value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #proman value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #servers value: nil)
 addChild: ((CxxTreeAssociation key: #schunk value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #sktsrv value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #srvloop value: #file) yourself); yourself);
 addChild: ((CxxTreeAssociation key: #transfers value: nil)
 addChild: ((CxxTreeAssociation key: #bin2com value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #cookbk value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #nxcvr value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #recipe value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #txtcomm value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #xfrspec value: #file) yourself); yourself); yourself);
 addChild: ((CxxTreeAssociation key: #disk value: #dir)
 addChild: ((CxxTreeAssociation key: #consist value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #diskman value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #dstat value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #fakedsk value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #flkinfo value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #packer value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #purging value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #snfinfo value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #turtle value: #file) yourself); yourself);
 addChild: ((CxxTreeAssociation key: 'server' value: #dir)
 addChild: ((CxxTreeAssociation key: #nfebe value: nil)
 addChild: ((CxxTreeAssociation key: #brange1 value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #brange2 value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #brange3 value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #crypto value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #detect value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #granmap value: #file) yourself);
 addChild: ((CxxTreeAssociation key: 'id' value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #nadmin value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #nkernel value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #nlinks value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #sysadm value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #wrapper value: #file) yourself); yourself);
 addChild: ((CxxTreeAssociation key: 'main' value: nil)
 addChild: ((CxxTreeAssociation key: 'sheph' value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #tclude value: #file) yourself); yourself);
 addChild: ((CxxTreeAssociation key: #ent value: nil)
 addChild: ((CxxTreeAssociation key: 'ent' value: #file) yourself);
 addChild: ((CxxTreeAssociation key: 'htree' value: #file) yourself);
 addChild: ((CxxTreeAssociation key: 'loaves' value: #file) yourself);
 addChild: ((CxxTreeAssociation key: 'oroot' value: #file) yourself); yourself);
 addChild: ((CxxTreeAssociation key: 'tracedag' value: nil)
 addChild: ((CxxTreeAssociation key: 'branch' value: #file) yourself);
 addChild: ((CxxTreeAssociation key: 'dagwood' value: #file) yourself);
 addChild: ((CxxTreeAssociation key: 'tracep' value: #file) yourself); yourself);
 addChild: ((CxxTreeAssociation key: 'entcanopy' value: nil)
 addChild: ((CxxTreeAssociation key: 'canopy' value: #file) yourself);
 addChild: ((CxxTreeAssociation key: 'props' value: #file) yourself); yourself);
 addChild: ((CxxTreeAssociation key: 'serverxpp' value: nil)
 addChild: ((CxxTreeAssociation key: 'counter' value: #file) yourself);
 addChild: ((CxxTreeAssociation key: 'grantab' value: #file) yourself); yourself);
 addChild: ((CxxTreeAssociation key: #'for febe' value: nil)
 addChild: ((CxxTreeAssociation key: #worksrv value: #file) yourself); yourself); yourself);
 addChild: ((CxxTreeAssociation key: 'xlatexpp' value: #dir)
 addChild: ((CxxTreeAssociation key: #lowlevel value: nil)
 addChild: ((CxxTreeAssociation key: 'become' value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #cache value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #gchooks value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #hlogger value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #tokens value: #file) yourself); yourself);
 addChild: ((CxxTreeAssociation key: #primtab value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #primval value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #rcmain value: #file) yourself);
 addChild: ((CxxTreeAssociation key: 'set' value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #settab value: #file) yourself);
 addChild: ((CxxTreeAssociation key: 'spaces' value: nil)
 addChild: ((CxxTreeAssociation key: #cross value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #edge value: #file) yourself);
 addChild: ((CxxTreeAssociation key: 'filter' value: #file) yourself);
 addChild: ((CxxTreeAssociation key: 'hspace' value: #file) yourself);
 addChild: ((CxxTreeAssociation key: 'integer' value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #real value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #sequenc value: #file) yourself);
 addChild: ((CxxTreeAssociation key: 'space' value: #file) yourself); yourself);
 addChild: ((CxxTreeAssociation key: 'stepper' value: #file) yourself);
 addChild: ((CxxTreeAssociation key: 'tabless' value: nil)
 addChild: ((CxxTreeAssociation key: 'array' value: #file) yourself);
 addChild: ((CxxTreeAssociation key: 'hashtab' value: #file) yourself);
 addChild: ((CxxTreeAssociation key: 'inttab' value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #tabent value: #file) yourself);
 addChild: ((CxxTreeAssociation key: 'tables' value: #file) yourself);
 addChild: ((CxxTreeAssociation key: #tabtool value: #file) yourself); yourself);
 addChild: ((CxxTreeAssociation key: 'tester' value: #file) yourself);
 addChild: ((CxxTreeAssociation key: 'thunk' value: #file) yourself); yourself); yourself); yourself))!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(XnBufferedWriteStream.class).setAttributes( new Set().add("CONCRETE").add("EQ").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public static void initializeSystemOrganization() {
	(CxxSystemOrganization.fileNamed("bootpln")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("bootpln")).addClassIn(AboraSupport.findAboraClass(BootPlan.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(Connection.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(BootMaker.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("bootpln")).addClassIn(AboraSupport.findAboraClass(NestedConnection.class).getOrMakeCxxClassDescription(), PROTECTED);
	(CxxSystemOrganization.getOrMakeFileNamed("bootpln")).addClassIn(AboraSupport.findAboraClass(ClearPlan.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(DirectConnection.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.fileNamed("comdtct")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("comdtct")).addClassIn(AboraSupport.findAboraClass(CommWaitDetector.class).getOrMakeCxxClassDescription(), PROTECTED).addClassIn(AboraSupport.findAboraClass(DetectorEvent.class).getOrMakeCxxClassDescription(), PROTECTED).addClassIn(AboraSupport.findAboraClass(CommRevisionDetector.class).getOrMakeCxxClassDescription(), PROTECTED).addClassIn(AboraSupport.findAboraClass(CommFillRangeDetector.class).getOrMakeCxxClassDescription(), PROTECTED).addClassIn(AboraSupport.findAboraClass(CommFillDetector.class).getOrMakeCxxClassDescription(), PROTECTED).addClassIn(AboraSupport.findAboraClass(CommStatusDetector.class).getOrMakeCxxClassDescription(), PROTECTED);
	(CxxSystemOrganization.getOrMakeFileNamed("comdtct")).cxxHeaderIn("#include \"shephx.hxx\"", PRIVATE);
	(CxxSystemOrganization.getOrMakeFileNamed("comdtct")).addClassIn(AboraSupport.findAboraClass(FilledEvent.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(GrabbedEvent.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(DoneEvent.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(RangeFilledEvent.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(ReleasedEvent.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(RevisedEvent.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.fileNamed("handlrs")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("handlrs")).hxxHeaderIn("typedef void (*VHHFn) (APTR(Heaper), APTR(Heaper));\n"+
"typedef void (*VHHHFn) (APTR(Heaper), APTR(Heaper), APTR(Heaper));\n"+
"typedef void (*VHFn) (APTR(Heaper));\n"+
"typedef void (*VHHHHFn) (APTR(Heaper), APTR(Heaper), APTR(Heaper), APTR(Heaper));\n"+
"typedef void (*VHHHHHFn) (APTR(Heaper), APTR(Heaper), APTR(Heaper), APTR(Heaper), APTR(Heaper));\n"+
"typedef void (*VHBFn) (APTR(Heaper), BooleanVar);\n"+
"typedef SPTR(Heaper) (*HFn) ();\n"+
"typedef SPTR(Heaper) (*HHFn) (APTR(Heaper));\n"+
"typedef SPTR(Heaper) (*HHHFn) (APTR(Heaper), APTR(Heaper));\n"+
"typedef SPTR(Heaper) (*HHHHFn) (APTR(Heaper), APTR(Heaper), APTR(Heaper));\n"+
"typedef SPTR(Heaper) (*HHHHHFn) (APTR(Heaper), APTR(Heaper), APTR(Heaper), APTR(Heaper));\n"+
"typedef SPTR(Heaper) (*HHHHHHFn) (APTR(Heaper), APTR(Heaper), APTR(Heaper), APTR(Heaper), APTR(Heaper));\n"+
"typedef SPTR(Heaper) (*HHHHHHHFn) (APTR(Heaper), APTR(Heaper), APTR(Heaper), APTR(Heaper), APTR(Heaper), APTR(Heaper));\n"+
"typedef BooleanVar (*BHHFn) (APTR(Heaper), APTR(Heaper));\n"+
"typedef BooleanVar (*BHFn) (APTR(Heaper));\n"+
"typedef SPTR(Heaper) (*HHBFn) (APTR(Heaper), BooleanVar);\n"+
"typedef SPTR(Heaper) (*HHHBFn) (APTR(Heaper), APTR(Heaper), BooleanVar);\n"+
"typedef BooleanVar (*BHHHFn) (APTR(Heaper), APTR(Heaper), APTR(Heaper));\n"+
"", PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("handlrs")).addClassIn(AboraSupport.findAboraClass(RequestHandler.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(HHHHHHHHandler.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(BHHandler.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(HHBHandler.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(HHHHandler.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(HHHBHandler.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(BHHHandler.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(HHHHHHHandler.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(VHBHandler.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(BHHHHandler.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(HHandler.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(HHHandler.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(VHHandler.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(VHHHHHHandler.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(HHHHHHandler.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(VHHHandler.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(HHHHHandler.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(VHHHHHandler.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(VHHHHandler.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.fileNamed("memstr")).comment("");
	(CxxSystemOrganization.fileNamed("negoti8")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("negoti8")).addClassIn(AboraSupport.findAboraClass(ProtocolBroker.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("negoti8")).addClassIn(AboraSupport.findAboraClass(ProtocolItem.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(SetDiskProtocol.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(SetCommProtocol.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.fileNamed("nscotty")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("nscotty")).hxxHeaderIn("", PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("nscotty")).cxxHeaderIn("#include <stdlib.h>\n"+
"", PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("nscotty")).addClassIn(AboraSupport.findAboraClass(XnWriteStream.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(WriteVariableArrayStream.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(XnReadStream.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("nscotty")).addClassIn(AboraSupport.findAboraClass(WriteMemStream.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(ReadArrayStream.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(WriteArrayStream.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(ReadMemStream.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.fileNamed("portal")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("portal")).addClassIn(AboraSupport.findAboraClass(Portal.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("portal")).addClassIn(AboraSupport.findAboraClass(PacketPortal.class).getOrMakeCxxClassDescription(), PROTECTED).addClassIn(AboraSupport.findAboraClass(XnBufferedReadStream.class).getOrMakeCxxClassDescription(), PROTECTED).addClassIn(AboraSupport.findAboraClass(PairPortal.class).getOrMakeCxxClassDescription(), PROTECTED);
	(CxxSystemOrganization.getOrMakeFileNamed("portal")).addClassIn(AboraSupport.findAboraClass(XnBufferedWriteStream.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.fileNamed("prolstn")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("prolstn")).cxxHeaderIn("#ifdef unix\n"+
"#	include \"sys/ioctl.h\"\n"+
"#	ifndef __sgi\n"+
"#		include \"sys/filio.h\"\n"+
"#	else\n"+
"#		include <osfcn.h>\n"+
"#	endif\n"+
"#endif\n"+
"", PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("prolstn")).addClassIn(AboraSupport.findAboraClass(IPPromiseListener.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("prolstn")).addClassIn(AboraSupport.findAboraClass(FePromiseSession.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.fileNamed("proman")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("proman")).hxxHeaderIn("typedef void (*VHFn) (APTR(Heaper));\n"+
"#define ERRLIST_10(a,b,c,d,e,f,g,h,i,j)	STR(a),ERRLIST_9(b,c,d,e,f,g,h,i,j)\n"+
"#include \"loggerx.hxx\"", PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("proman")).addClassIn(AboraSupport.findAboraClass(ExampleHIHHandler.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(PromiseManager.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("proman")).addClassIn(AboraSupport.findAboraClass(ByteShuffler.class).getOrMakeCxxClassDescription(), PROTECTED).addClassIn(AboraSupport.findAboraClass(ExecutePromiseFile.class).getOrMakeCxxClassDescription(), PROTECTED);
	(CxxSystemOrganization.getOrMakeFileNamed("proman")).cxxHeaderIn("#include \"shephx.hxx\"\n"+
"", PRIVATE);
	(CxxSystemOrganization.getOrMakeFileNamed("proman")).addClassIn(AboraSupport.findAboraClass(NoShuffler.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(SimpleShuffler.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(ExceptionRecord.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(SpecialHandler.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.getOrMakeFileNamed("proman")).addClassIn(AboraSupport.findAboraClass(ShuffleTester.class).getOrMakeCxxClassDescription(), TEST);
	(CxxSystemOrganization.fileNamed("schunk")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("schunk")).cxxHeaderIn("#include <stdlib.h>\n"+
"#include \"allocx.hxx\"", PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("schunk")).addClassIn(AboraSupport.findAboraClass(ServerChunk.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(ChunkCleaner.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("schunk")).addClassIn(AboraSupport.findAboraClass(ListenerEmulsion.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.getOrMakeFileNamed("schunk")).addClassIn(AboraSupport.findAboraClass(TestChunk.class).getOrMakeCxxClassDescription(), TEST);
	(CxxSystemOrganization.fileNamed("sktsrv")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("sktsrv")).cxxHeaderIn("#include <sys/types.h>\n"+
"#if defined(HIGHC) | defined(_MSC_VER)\n"+
"#	include <sys/time.h>\n"+
"#endif /* HIGHC | _MSC_VER */\n"+
"#include <stdlib.h>\n"+
"#include <stream.h>\n"+
"#include <string.h>\n"+
"#include <fcntl.h>\n"+
"\n"+
"#ifdef unix\n"+
"#	include <netdbx.hxx>\n"+
"#	include <sys/socket.h>\n"+
"#	include <netinet/in.h>\n"+
"#	include <sys/socket.h>\n"+
"#	include <osfcn.h>\n"+
"#	ifdef __sgi\n"+
"		int	getdtablesize();		/* SGI forgot to put it in osfcn.h */\n"+
"#		include <libc.h>	/* for bzero(), which is called by FD_ZERO */\n"+
"#		include <errno.h>\n"+
"#	endif	/* sgi */\n"+
"#endif /* unix */\n"+
"\n"+
"#include <signal.h>\n"+
"\n"+
"#ifdef WIN32\n"+
"#	include <winsock.h>\n"+
"#	include <io.h>\n"+
"#	define close _close\n"+
"#endif /* WIN32 */\n"+
"\n"+
"#ifdef HIGHC\n"+
"extern \"C\" {\n"+
"#	define NOMEMMGR /* TO AVOID GPTR MACRO CONFLICT */\n"+
"#	include <nmpcip.h>\n"+
"};\n"+
"#endif /* HIGHC */\n"+
"#include <socketx.hxx>\n"+
"", PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("sktsrv")).addClassIn(AboraSupport.findAboraClass(FDListener.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("sktsrv")).hxxHeaderIn("#ifdef WIN32\n"+
"#	include <fdset.h>\n"+
"#else\n"+
"#ifdef unix\n"+
"#	include <sys/time.h>\n"+
"#	include <unistd.h>\n"+
"#endif /* unix */\n"+
"#endif /* WIN32 */\n"+
"#include <sys/types.h>", PRIVATE);
	(CxxSystemOrganization.getOrMakeFileNamed("sktsrv")).cxxHeaderIn("#ifdef WIN32\n"+
"#	include <io.h>\n"+
"#	include <winsock.h>\n"+
"#else\n"+
"#ifdef unix\n"+
"#	ifndef __sgi\n"+
"#		include <sys/filio.h>\n"+
"#	endif /* __sgi */\n"+
"#endif /* unix */\n"+
"#endif /* WIN32 */", PRIVATE);
	(CxxSystemOrganization.getOrMakeFileNamed("sktsrv")).addClassIn(AboraSupport.findAboraClass(SelectServerLoop.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(IPRendezvousListener.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.fileNamed("srvloop")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("srvloop")).hxxHeaderIn("", PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("srvloop")).cxxHeaderIn("#include <stdlib.h>", PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("srvloop")).addClassIn(AboraSupport.findAboraClass(ServerLoop.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.fileNamed("bin2com")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("bin2com")).addClassIn(AboraSupport.findAboraClass(Binary2XcvrMaker.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("bin2com")).addClassIn(AboraSupport.findAboraClass(Binary2Xmtr.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(Binary2Rcvr.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.fileNamed("cookbk")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("cookbk")).addClassIn(AboraSupport.findAboraClass(Cookbook.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("cookbk")).addClassIn(AboraSupport.findAboraClass(ActualCookbook.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.fileNamed("nxcvr")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("nxcvr")).hxxHeaderIn("", PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("nxcvr")).addClassIn(AboraSupport.findAboraClass(Rcvr.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(Xmtr.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.fileNamed("recipe")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("recipe")).hxxHeaderIn("#include \"parrayx.oxx\" // definition for friend function in Recipe", PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("recipe")).addClassIn(AboraSupport.findAboraClass(Recipe.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(CopyRecipe.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(StubRecipe.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.fileNamed("txtcomm")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("txtcomm")).hxxHeaderIn("#define CONVERTSTRLEN 16", PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("txtcomm")).addClassIn(AboraSupport.findAboraClass(TextyXcvrMaker.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("txtcomm")).cxxHeaderIn("#include <string.h>\n"+
"#include <ctype.h>", PRIVATE);
	(CxxSystemOrganization.getOrMakeFileNamed("txtcomm")).addClassIn(AboraSupport.findAboraClass(TextyRcvr.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(TextyXmtr.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.fileNamed("xfrspec")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("xfrspec")).hxxHeaderIn("#include \"choosex.hxx\"", PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("xfrspec")).cxxHeaderIn("#include \"tofup.hxx\"\n"+
"#include \"fhashx.hxx\"\n"+
"#include <string.h>", PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("xfrspec")).addClassIn(AboraSupport.findAboraClass(XcvrMaker.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(SpecialistRcvr.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(TransferSpecialist.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(SpecialistXmtr.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("xfrspec")).cxxHeaderIn("#include \"copyrcpx.hxx\"\n"+
"extern Recipe * XppCuisine;\n"+
"", PRIVATE);
	(CxxSystemOrganization.getOrMakeFileNamed("xfrspec")).addClassIn(AboraSupport.findAboraClass(CommIbid.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(BogusXcvrMaker.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(TransferGeneralist.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(CategoryRecipe.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.fileNamed("consist")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("consist")).addClassIn(AboraSupport.findAboraClass(CBlockTrackingPacker.class).getOrMakeCxxClassDescription(), TEST).addClassIn(AboraSupport.findAboraClass(PrintCBlocksTracks.class).getOrMakeCxxClassDescription(), TEST).addClassIn(AboraSupport.findAboraClass(TrackCBlocks.class).getOrMakeCxxClassDescription(), TEST).addClassIn(AboraSupport.findAboraClass(CBlockTracker.class).getOrMakeCxxClassDescription(), TEST);
	(CxxSystemOrganization.fileNamed("diskman")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("diskman")).hxxHeaderIn("#define BEGIN_CONSISTENT(dirty)												\\\n"+
"	{	CurrentPacker.fluidGet()->beginConsistent(dirty);						\\\n"+
"		CurrentPacker.fluidGet()->consistentBlockAt(__FILE__,__LINE__);	\\\n"+
"		PLANT_BOMB(ConsistentBlock,Boom);									\\\n"+
"		ARM_BOMB(Boom,(dirty));												\\\n"+
"		{																		\\\n"+
"			FLUID_BIND(InsideTransactionFlag,TRUE)  {\n"+
"	\n"+
"#define END_CONSISTENT	}	}	}\n"+
"\n"+
"#define BEGIN_INSISTENT(dirty)													\\\n"+
"	{	if (! InsideTransactionFlag.fluidFetch()) {									\\\n"+
"			BLAST(Assertion_failed);												\\\n"+
"		}																			\\\n"+
"		CurrentPacker.fluidGet()->beginConsistent(dirty);						\\\n"+
"		CurrentPacker.fluidGet()->consistentBlockAt(__FILE__,__LINE__);	\\\n"+
"		PLANT_BOMB(ConsistentBlock,Boom);									\\\n"+
"		ARM_BOMB(Boom,(dirty));												\\\n"+
"		{\n"+
"	\n"+
"#define END_INSISTENT	}	}\n"+
"\n"+
"", PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("diskman")).cxxHeaderIn("#include \"allocx.hxx\"", PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("diskman")).addClassIn(AboraSupport.findAboraClass(DiskManager.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(ShepherdBootMaker.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("diskman")).cxxHeaderIn("#include <stdlib.h>", PRIVATE);
	(CxxSystemOrganization.getOrMakeFileNamed("diskman")).addClassIn(AboraSupport.findAboraClass(FromDiskPlan.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(DiskManagerEmulsion.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(DiskConnection.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(Cattleman.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.getOrMakeFileNamed("diskman")).addClassIn(AboraSupport.findAboraClass(DiskTester.class).getOrMakeCxxClassDescription(), TEST).addClassIn(AboraSupport.findAboraClass(MultiCounter.class).getOrMakeCxxClassDescription(), TEST);
	(CxxSystemOrganization.fileNamed("dstat")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("dstat")).addClassIn(AboraSupport.findAboraClass(SnarfStatistics.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(SpecialistRcvrJig.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.fileNamed("fakedsk")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("fakedsk")).addClassIn(AboraSupport.findAboraClass(FakeDisk.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(FakePacker.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(MockTurtle.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.fileNamed("flkinfo")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("flkinfo")).addClassIn(AboraSupport.findAboraClass(FlockLocation.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(FlockInfo.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.fileNamed("packer")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("packer")).addClassIn(AboraSupport.findAboraClass(SnarfPacker.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("packer")).hxxHeaderIn("", PRIVATE);
	(CxxSystemOrganization.getOrMakeFileNamed("packer")).cxxHeaderIn("#include <string.h>", PRIVATE);
	(CxxSystemOrganization.getOrMakeFileNamed("packer")).addClassIn(AboraSupport.findAboraClass(Pumpkin.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(PersistentCleaner.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(DiskCountSpecialist.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(SnarfRecord.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(DiskIniter.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(SpareStageSpace.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(DiskSpecialist.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(CountStream.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.getOrMakeFileNamed("packer")).addClassIn(AboraSupport.findAboraClass(TestFlockInfo.class).getOrMakeCxxClassDescription(), TEST).addClassIn(AboraSupport.findAboraClass(Honestly.class).getOrMakeCxxClassDescription(), TEST).addClassIn(AboraSupport.findAboraClass(PairFlock.class).getOrMakeCxxClassDescription(), TEST).addClassIn(AboraSupport.findAboraClass(HashStream.class).getOrMakeCxxClassDescription(), TEST).addClassIn(AboraSupport.findAboraClass(DoublingFlock.class).getOrMakeCxxClassDescription(), TEST).addClassIn(AboraSupport.findAboraClass(HonestAbePlan.class).getOrMakeCxxClassDescription(), TEST).addClassIn(AboraSupport.findAboraClass(HonestAbeIniter.class).getOrMakeCxxClassDescription(), TEST).addClassIn(AboraSupport.findAboraClass(TestPacker.class).getOrMakeCxxClassDescription(), TEST);
	(CxxSystemOrganization.fileNamed("purging")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("purging")).addClassIn(AboraSupport.findAboraClass(LiberalPurgeror.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(Purgeror.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("purging")).addClassIn(AboraSupport.findAboraClass(DiskPurgeRate.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.fileNamed("snfinfo")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("snfinfo")).addClassIn(AboraSupport.findAboraClass(SnarfInfoHandler.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(SnarfHandler.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.fileNamed("turtle")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("turtle")).addClassIn(AboraSupport.findAboraClass(Turtle.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(AgendaItem.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(Sequencer.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(Agenda.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("turtle")).addClassIn(AboraSupport.findAboraClass(SimpleTurtle.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.fileNamed("brange1")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("brange1")).addClassIn(AboraSupport.findAboraClass(BeCarrier.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(BeRangeElement.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(BePlaceHolder.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(BeLabel.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(BeIDHolder.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(BeDataHolder.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("brange1")).addClassIn(AboraSupport.findAboraClass(FillDetectorExecutor.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.fileNamed("brange2")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("brange2")).cxxHeaderIn("#include \"entx.hxx\"  // for various fluids.", PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("brange2")).addClassIn(AboraSupport.findAboraClass(BeWork.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(BeClub.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("brange2")).addClassIn(AboraSupport.findAboraClass(BeWorkLockExecutor.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(UpdateTransitiveSuperClubIDs.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(UpdateTransitiveMemberIDs.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(RevisionWatcherExecutor.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.fileNamed("brange3")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("brange3")).addClassIn(AboraSupport.findAboraClass(BeEdition.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("brange3")).addClassIn(AboraSupport.findAboraClass(BeEditionDetectorExecutor.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.fileNamed("crypto")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("crypto")).hxxHeaderIn("typedef SPTR(Encrypter) (*EncrypterConstructor) (APTR(UInt8Array) OR(NULL) publicKey, APTR(UInt8Array) OR(NULL) privateKey);\n"+
"\n"+
"#define DEFINE_ENCRYPTER(identifier,encryptorClass) {		\\\n"+
"	REQUIRES(Encrypter);			\\\n"+
"	Encrypter::remember(Sequence::string(identifier), encryptorClass::make);			\\\n"+
"}\n"+
"\n"+
"#define DEFINE_SCRAMBLER(identifier,scrambler) {	\\\n"+
"	REQUIRES(Scrambler);			\\\n"+
"	Scrambler::remember(Sequence::string(identifier), scrambler);				\\\n"+
"}", PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("crypto")).addClassIn(AboraSupport.findAboraClass(Encrypter.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(Scrambler.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("crypto")).addClassIn(AboraSupport.findAboraClass(NoScrambler.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(EncrypterMaker.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(NoEncrypter.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.fileNamed("detect")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("detect")).addClassIn(AboraSupport.findAboraClass(FeDetector.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(FeFillDetector.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(FeFillRangeDetector.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(FeRevisionDetector.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(FeStatusDetector.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(FeWaitDetector.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.fileNamed("granmap")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("granmap")).addClassIn(AboraSupport.findAboraClass(BeGrandMap.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("granmap")).addClassIn(AboraSupport.findAboraClass(GrantStepper.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(BackendBootMaker.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.fileNamed("id")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("id")).addClassIn(AboraSupport.findAboraClass(IDRegion.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(IDSpace.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(IDDsp.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(ID.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("id")).addClassIn(AboraSupport.findAboraClass(IDUpOrder.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(IDStepper.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(IDSimpleStepper.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.getOrMakeFileNamed("id")).addClassIn(AboraSupport.findAboraClass(IDTester.class).getOrMakeCxxClassDescription(), TEST);
	(CxxSystemOrganization.fileNamed("nadmin")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("nadmin")).addClassIn(AboraSupport.findAboraClass(Lock.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(FeLockSmith.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(MatchLock.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(FeClubDescription.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(FeBooLockSmith.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(MultiLock.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(ChallengeLock.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(WallLock.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(FeChallengeLockSmith.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(FeWallLockSmith.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(FeSession.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(BooLock.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(FeMatchLockSmith.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(FeMultiLockSmith.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("nadmin")).addClassIn(AboraSupport.findAboraClass(DefaultSession.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.fileNamed("nkernel")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("nkernel")).hxxHeaderIn("#define NOACK void", PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("nkernel")).cxxHeaderIn("#include \"choosex.hxx\"\n"+
"", PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("nkernel")).addClassIn(AboraSupport.findAboraClass(FeRangeElement.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(FeBundle.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(FeLabel.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(FeArrayBundle.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(FePlaceHolderBundle.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(FePlaceHolder.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(FeEdition.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(FeWork.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(FeDataHolder.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(FeServer.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(FeClub.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(FeKeyMaster.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(FeIDHolder.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(FeElementBundle.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("nkernel")).addClassIn(AboraSupport.findAboraClass(FeVirtualDataHolder.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(FeGrandPlaceHolder.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(RevisionDetectorExecutor.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(FeActualPlaceHolder.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(EditionStepper.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(FeVirtualPlaceHolder.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(StatusDetectorExecutor.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(FeActualDataHolder.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.getOrMakeFileNamed("nkernel")).addClassIn(AboraSupport.findAboraClass(WorksTestFillDetector.class).getOrMakeCxxClassDescription(), TEST).addClassIn(AboraSupport.findAboraClass(WorksTestStatusDetector.class).getOrMakeCxxClassDescription(), TEST).addClassIn(AboraSupport.findAboraClass(WorksTester.class).getOrMakeCxxClassDescription(), TEST).addClassIn(AboraSupport.findAboraClass(VolumeTester.class).getOrMakeCxxClassDescription(), TEST).addClassIn(AboraSupport.findAboraClass(WorksTestFillRangeDetector.class).getOrMakeCxxClassDescription(), TEST);
	(CxxSystemOrganization.fileNamed("nlinks")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("nlinks")).addClassIn(AboraSupport.findAboraClass(FeHyperRef.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(FeSingleRef.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(FeHyperLink.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(FePath.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(FeMultiRef.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.fileNamed("sysadm")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("sysadm")).addClassIn(AboraSupport.findAboraClass(FeArchiver.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(FeAdminer.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.fileNamed("wrapper")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("wrapper")).hxxHeaderIn("/* Function pointer types for wrappers */\n"+
"\n"+
"typedef void (*FeWrapperSpecHolder) (APTR(FeWrapperSpec));\n"+
"typedef SPTR(FeWrapper) (*FeDirectWrapperMaker) (APTR(FeEdition));\n"+
"typedef SPTR(FeWrapper) (*FeIndirectWrapperMaker) (APTR(FeEdition), APTR(FeWrapper));\n"+
"typedef BooleanVar (*FeDirectWrapperChecker) (APTR(FeEdition));\n"+
"typedef BooleanVar (*FeIndirectWrapperChecker) (APTR(FeEdition));\n"+
"\n"+
"#define ABSTRACTWRAPPER(wrapperName,superName,className) \\\n"+
"	REQUIRES(Sequence); \\\n"+
"	REQUIRES(FeWrapperSpec); \\\n"+
"	FeWrapperSpec::registerAbstract (wrapperName, superName, className::setSpec)\n"+
"\n"+
"#define DIRECTWRAPPER(wrapperName,superName,className) \\\n"+
"	REQUIRES(Sequence); \\\n"+
"	REQUIRES(FeWrapperSpec); \\\n"+
"	FeWrapperSpec::registerDirect (wrapperName, superName, \\\n"+
"		className::makeWrapper, className::check, className::setSpec)\n"+
"		\n"+
"#define INDIRECTWRAPPER(wrapperName,superName,innerName,className) \\\n"+
"	REQUIRES(Sequence); \\\n"+
"	REQUIRES(FeWrapperSpec); \\\n"+
"	FeWrapperSpec::registerDirect (wrapperName, superName, innerName, \\\n"+
"		className::makeWrapper, className::check, className::setSpec)\n"+
"\n"+
"", PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("wrapper")).addClassIn(AboraSupport.findAboraClass(FeWrapperSpec.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(FeWrapper.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(FeSet.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(FeText.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(FeWorkSet.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("wrapper")).addClassIn(AboraSupport.findAboraClass(FeConcreteWrapperSpec.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(FeIndirectWrapperSpec.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(FeDirectWrapperSpec.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(FeWrapperDef.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(FeAbstractWrapperSpec.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(FeAbstractWrapperDef.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(FeIndirectWrapperDef.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(FeDirectWrapperDef.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.fileNamed("sheph")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("sheph")).hxxHeaderIn("class ShepFlag {};\n"+
"#ifdef GNU\n"+
"extern ShepFlag shepFlag;\n"+
"#else\n"+
"const ShepFlag shepFlag;\n"+
"#endif\n"+
"#ifndef STUBBLE\n"+
"\n"+
"/* Prototype Constructor Identification Junk */\n"+
"\n"+
"class PCIJ {};\n"+
"#ifdef GNU\n"+
"extern PCIJ pcij;\n"+
"#else\n"+
"const PCIJ pcij;\n"+
"#endif\n"+
"/* Used to identify calls on a constructor intended to be used only  */\n"+
"/* to create prototypes for the use of changeClassToThatOf() */\n"+
"\n"+
"#define LOCKED(className)						\\\n"+
"  public:								\\\n"+
"    className(PCIJ);							\\\n"+
"  private:\n"+
"\n"+
"#define DEFERRED_LOCKED(className)					\\\n"+
"  public:								\\\n"+
"    className(PCIJ);							\\\n"+
"  private:\n"+
"\n"+
"#define NOFAULT\n"+
"\n"+
"#define NOLOCK\n"+
"\n"+
"/* ========================================================================== */\n"+
"//\n"+
"// Attribute macros.  (Class must also be a COPY() class.)\n"+
"//\n"+
"//	SHEPHERD_PATRIARCH()	tells stubble to generate a shepherd stub and\n"+
"//				falls through to SHEPHERD_ANCESTOR\n"+
"//\n"+
"//	SHEPHERD_ANCESTOR()	generates a constructor for passing the\n"+
"//				hash to Abraham during stub creation\n"+
"//\n"+
"// Rules for their use:\n"+
"//\n"+
"//	- All (abstract or concrete) classes which inherit from Abraham\n"+
"//	  are \"shepherds\".\n"+
"//\n"+
"//	- Shepherds must be COPY() classes.\n"+
"//\n"+
"//	- Every concrete shepherd class must either be, or inherit from,\n"+
"//	  a class with the SHEPHERD_PATRIARCH() attribute (called a\n"+
"//	  SHEPHERD_PATRIARCH class.)\n"+
"//\n"+
"//	- Every class between a SHEPHERD_PATRIARCH and Abraham must have\n"+
"//	  either a SHEPHERD_PATRIARCH() or a SHEPHERD_ANCESTOR() attribute.\n"+
"//\n"+
"//	- (Thus, the SHEPHERD_ANCESTOR() attribute is optional in classes\n"+
"//	  below the last SHEPHERD_PATRIARCH.  Giving them the attribute\n"+
"//	  causes extra code to be generated, allowing you to define\n"+
"//	  another SHEPHERD_PATRIARCH which inherits from them.)\n"+
"//\n"+
"//  - In order to make becomeStub faster, I've made the stubbing\n"+
"//    constructor inline.  It has to be here, and not in the sxx file\n"+
"//    so that subclasses outside of the module defining the ANCESTOR\n"+
"//    can see it.  Note that for I use the implicit superclass name for\n"+
"//    the constructor, and therein rely on single inheritance.  An\n"+
"//    alternative is to have SHEPHERD_ANCESTOR specify its superclass\n"+
"//		- ech 3-19-92\n"+
"//\n"+
"//  - Put back non-inline variant of SHEPHERD_ANCESTOR for use when\n"+
"//     inlining is turned off.\n"+
"//		- ech 4-3-92\n"+
"/* ========================================================================== */\n"+
"\n"+
"#define SHEPHERD_PATRIARCH(className,baseClassName)					\\\n"+
"        public: 							\\\n"+
"	   SPTR(Category) getShepherdStubCategory() CONST;			\\\n"+
"	   void becomeStub();								\\\n"+
"	SHEPHERD_ANCESTOR(className,baseClassName)\n"+
"\n"+
"#ifdef USE_INLINE\n"+
"\n"+
"#define SHEPHERD_ANCESTOR(className,baseClassName)					\\\n"+
"     protected: 							\\\n"+
"	   inline className(ShepFlag /*aFlag*/, UInt32 aHash, APTR(FlockInfo) info) \\\n"+
"	   			: baseClassName(shepFlag, aHash, info) {}				\\\n"+
"	private:\n"+
"\n"+
"#else\n"+
"/* the constructor definitition in this case is in the .sxx file */\n"+
"#define SHEPHERD_ANCESTOR(className)					\\\n"+
"     protected: 							\\\n"+
"	   className(ShepFlag aFlag, UInt32 aHash, APTR(FlockInfo) info);		\\\n"+
"	private:\n"+
"#endif /* USE_INLINE */\n"+
"\n"+
"#endif /* STUBBLE */\n"+
"", PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("sheph")).addClassIn(AboraSupport.findAboraClass(Abraham.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("sheph")).addClassIn(AboraSupport.findAboraClass(ShepherdLocked.class).getOrMakeCxxClassDescription(), TEST).addClassIn(AboraSupport.findAboraClass(ShepherdLockTester.class).getOrMakeCxxClassDescription(), TEST);
	(CxxSystemOrganization.fileNamed("tclude")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("tclude")).hxxHeaderIn("/* Should only be called if I am not extinct. */\n"+
"\n"+
"#define BEGIN_REANIMATE(fossil,Type,var)				\\\n"+
"	{							\\\n"+
"		SPTR(Type) var = CAST(Type,(fossil)->secretRecorder());	\\\n"+
"		PLANT_BOMB(ReleaseRecorder,Boom);			\\\n"+
"		ARM_BOMB(Boom,&*(fossil));			\\\n"+
"		{\n"+
"		\n"+
"#define END_REANIMATE	}	}", PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("tclude")).addClassIn(AboraSupport.findAboraClass(RecorderTrigger.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(ResultRecorder.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(Matcher.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(TrailBlazer.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(NorthRecorderChecker.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(HashSetCache.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(RecorderFossil.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(SouthRecorderChecker.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(WorkRecorder.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(EditionRecorder.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(RecorderHoister.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("tclude")).addClassIn(AboraSupport.findAboraClass(EditionRecorderFossil.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(DirectEditionRecorder.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(IndirectWorkRecorder.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(WorkRecorderFossil.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(DirectEditionRecorderFossil.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(DirectWorkRecorder.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(IndirectWorkRecorderFossil.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(DirectWorkRecorderFossil.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(IndirectEditionRecorderFossil.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(IndirectEditionRecorder.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.fileNamed("ent")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("ent")).addClassIn(AboraSupport.findAboraClass(Ent.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.fileNamed("htree")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("htree")).addClassIn(AboraSupport.findAboraClass(HistoryCrum.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(HUpperCrum.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.fileNamed("loaves")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("loaves")).addClassIn(AboraSupport.findAboraClass(Loaf.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(OExpandingLoaf.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(InnerLoaf.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("loaves")).addClassIn(AboraSupport.findAboraClass(MergeBundlesStepper.class).getOrMakeCxxClassDescription(), PROTECTED);
	(CxxSystemOrganization.getOrMakeFileNamed("loaves")).addClassIn(AboraSupport.findAboraClass(DspLoaf.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(SplitLoaf.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(OVirtualLoaf.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(SharedData.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(RegionLoaf.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(OPartialLoaf.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.fileNamed("oroot")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("oroot")).addClassIn(AboraSupport.findAboraClass(OPart.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(OrglRoot.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(ActualOrglRoot.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(EmptyOrglRoot.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("oroot")).addClassIn(AboraSupport.findAboraClass(HBottomCrum.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.fileNamed("branch")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("branch")).hxxHeaderIn("", PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("branch")).addClassIn(AboraSupport.findAboraClass(BranchDescription.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(DagBranch.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(TreeBranch.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(RootBranch.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.fileNamed("dagwood")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("dagwood")).addClassIn(AboraSupport.findAboraClass(DagWood.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.fileNamed("tracep")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("tracep")).addClassIn(AboraSupport.findAboraClass(TracePosition.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(BoundedTrace.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.fileNamed("canopy")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("canopy")).addClassIn(AboraSupport.findAboraClass(CanopyCrum.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(SensorCrum.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(BertCrum.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("canopy")).addClassIn(AboraSupport.findAboraClass(PropChanger.class).getOrMakeCxxClassDescription(), PROTECTED).addClassIn(AboraSupport.findAboraClass(ActualPropChanger.class).getOrMakeCxxClassDescription(), PROTECTED);
	(CxxSystemOrganization.getOrMakeFileNamed("canopy")).addClassIn(AboraSupport.findAboraClass(HeightChanger.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(Heaper2UInt32Cache.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(CanopyCache.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.fileNamed("props")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("props")).addClassIn(AboraSupport.findAboraClass(PropFinder.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(Prop.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(BertProp.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(SensorProp.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(PropChange.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("props")).addClassIn(AboraSupport.findAboraClass(CannotPartializeChange.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(DetectorWaitingChange.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(FullPropChange.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(SensorPropFinder.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(OpenPropFinder.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(BertPropChange.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(EndorsementsChange.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(AbstractRecorderFinder.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(ClosedPropFinder.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(PermissionsChange.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(BertPropFinder.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(SimpleRecorderFinder.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(AnyRecorderFinder.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(ContainedEditionRecorderEFinder.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(ResultRecorderPFinder.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(PartialityFinder.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(BackfollowFinder.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(AnyRecorderEFinder.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(CannotPartializeFinder.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(SensorPropChange.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(SensorFinder.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(CumulativeRecorderFinder.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(BackfollowPFinder.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(AnyRecorderPFinder.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(OriginalResultRecorderEFinder.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.fileNamed("counter")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("counter")).addClassIn(AboraSupport.findAboraClass(Counter.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("counter")).addClassIn(AboraSupport.findAboraClass(BatchCounter.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(SingleCounter.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.fileNamed("grantab")).comment("Presently the values called 'shift' in this module are used with\n"+
"divide and modulo operations rather than bit operations.  Thus\n"+
"the minimum shift for a hashed key is 1 and not 0.");
	(CxxSystemOrganization.getOrMakeFileNamed("grantab")).hxxHeaderIn("", PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("grantab")).cxxHeaderIn("#include <math.h>\n"+
"", PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("grantab")).addClassIn(AboraSupport.findAboraClass(GrandHashTable.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(GrandHashSet.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("grantab")).hxxHeaderIn("#include \"fhashx.hxx\"", PRIVATE);
	(CxxSystemOrganization.getOrMakeFileNamed("grantab")).cxxHeaderIn("#include \"fhashx.hxx\"\n"+
"", PRIVATE);
	(CxxSystemOrganization.getOrMakeFileNamed("grantab")).addClassIn(AboraSupport.findAboraClass(GrandHashSetStepper.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(GrandOverflowStepper.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(GrandNodeReinserter.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(GrandHashTableStepper.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(GrandNodeDoubler.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(GrandEntry.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(GrandTableEntry.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(GrandDataPage.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(GrandDataPageStepper.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(GrandOverflow.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(GrandSetEntry.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(GrandNodeStepper.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(GrandNode.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(ExponentialHashMap.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.getOrMakeFileNamed("grantab")).hxxHeaderIn("#include <stream.h>", TEST);
	(CxxSystemOrganization.getOrMakeFileNamed("grantab")).addClassIn(AboraSupport.findAboraClass(GrandHashTableTester.class).getOrMakeCxxClassDescription(), TEST).addClassIn(AboraSupport.findAboraClass(GrandHashSetTester.class).getOrMakeCxxClassDescription(), TEST);
	(CxxSystemOrganization.fileNamed("worksrv")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("worksrv")).hxxHeaderIn("#define NOACK void", PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("worksrv")).addClassIn(AboraSupport.findAboraClass(WorksBootMaker.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(WorksIniter.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(FeWorksBootMaker.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(WorksWaitDetector.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.fileNamed("become")).comment("This module exists purely to test \"become\" support.  \n"+
"The implememtation of \"become\" support is elsewhere (in tofu & init).");
	(CxxSystemOrganization.getOrMakeFileNamed("become")).addClassIn(AboraSupport.findAboraClass(BecomeTester.class).getOrMakeCxxClassDescription(), TEST).addClassIn(AboraSupport.findAboraClass(Chameleon.class).getOrMakeCxxClassDescription(), TEST).addClassIn(AboraSupport.findAboraClass(Butterfly.class).getOrMakeCxxClassDescription(), TEST).addClassIn(AboraSupport.findAboraClass(DeadMoth.class).getOrMakeCxxClassDescription(), TEST).addClassIn(AboraSupport.findAboraClass(Moth.class).getOrMakeCxxClassDescription(), TEST).addClassIn(AboraSupport.findAboraClass(IronButterfly.class).getOrMakeCxxClassDescription(), TEST).addClassIn(AboraSupport.findAboraClass(GoldButterfly.class).getOrMakeCxxClassDescription(), TEST).addClassIn(AboraSupport.findAboraClass(LeadButterfly.class).getOrMakeCxxClassDescription(), TEST).addClassIn(AboraSupport.findAboraClass(DeadButterfly.class).getOrMakeCxxClassDescription(), TEST);
	(CxxSystemOrganization.fileNamed("cache")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("cache")).addClassIn(AboraSupport.findAboraClass(InstanceCache.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(CacheManager.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("cache")).addClassIn(AboraSupport.findAboraClass(SuspendedHeaper.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.fileNamed("gchooks")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("gchooks")).cxxHeaderIn("#include <osfcn.h>\n"+
"#include <stdlib.h>", PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("gchooks")).addClassIn(AboraSupport.findAboraClass(StackExaminer.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(SanitationEngineer.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(DeleteExecutor.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(RepairEngineer.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(CloseExecutor.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.fileNamed("hlogger")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("hlogger")).addClassIn(AboraSupport.findAboraClass(SwitchLogger.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.getOrMakeFileNamed("hlogger")).addClassIn(AboraSupport.findAboraClass(LogTester.class).getOrMakeCxxClassDescription(), TEST);
	(CxxSystemOrganization.fileNamed("tokens")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("tokens")).addClassIn(AboraSupport.findAboraClass(TokenSource.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.fileNamed("primtab")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("primtab")).cxxHeaderIn("#include \"fhashx.hxx\"", PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("primtab")).addClassIn(AboraSupport.findAboraClass(PrimIndexTable.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(PrimIndexTableStepper.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(PrimPtr2PtrTable.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(PrimSet.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(PrimPtrTableStepper.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(PrimPtr2PtrTableStepper.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(PrimPtrTable.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("primtab")).addClassIn(AboraSupport.findAboraClass(PrimRemovedObject.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(PrimPtrTableExecutor.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(PrimSetExecutor.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(PrimSetStepper.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.getOrMakeFileNamed("primtab")).addClassIn(AboraSupport.findAboraClass(PrimPtrTableTester.class).getOrMakeCxxClassDescription(), TEST).addClassIn(AboraSupport.findAboraClass(PrimIndexTableTester.class).getOrMakeCxxClassDescription(), TEST);
	(CxxSystemOrganization.fileNamed("primval")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("primval")).cxxHeaderIn("#include <math.h>\n"+
"", PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("primval")).addClassIn(AboraSupport.findAboraClass(PrimSpec.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(PrimIntegerSpec.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(PrimFloatSpec.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(PrimValue.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(PrimFloatValue.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(PrimIEEE64.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(PrimIntValue.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(PrimPointerSpec.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(PrimIEEE32.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.fileNamed("rcmain")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("rcmain")).addClassIn(AboraSupport.findAboraClass(MainDummy.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.fileNamed("set")).comment("This file group has no comment");
	(CxxSystemOrganization.getOrMakeFileNamed("set")).hxxHeaderIn("/* xpp class */\n"+
"", PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("set")).commentIn("This group of files represents the Set type of collections for X++.  There are 3\n"+
"basic categories:\n"+
"	 ScruSet - which is read-only but is not guaranteed to not change,\n"+
"	 ImmuSet - a set which is guaranteed not to change once constructed, and\n"+
"	 MuSet - a set which has a protocol to allow it to be changed.\n"+
"", PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("set")).addClassIn(AboraSupport.findAboraClass(ScruSet.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(UnionRecruiter.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(ImmuSet.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(MuSet.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(SetAccumulator.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("set")).addClassIn(AboraSupport.findAboraClass(ImmuSetOnMu.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(HashSet.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(EmptyImmuSet.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(ActualHashSet.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(TinyImmuSet.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(HashSetStepper.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.getOrMakeFileNamed("set")).hxxHeaderIn("\n"+
"#include <stream.h>", TEST);
	(CxxSystemOrganization.getOrMakeFileNamed("set")).cxxHeaderIn("#include \"setp.hxx\"\n"+
"#include \"tablesx.hxx\"\n"+
"#include \"choosex.hxx\"", TEST);
	(CxxSystemOrganization.getOrMakeFileNamed("set")).addClassIn(AboraSupport.findAboraClass(ScruSetTester.class).getOrMakeCxxClassDescription(), TEST).addClassIn(AboraSupport.findAboraClass(SetTester.class).getOrMakeCxxClassDescription(), TEST).addClassIn(AboraSupport.findAboraClass(MuSetTester.class).getOrMakeCxxClassDescription(), TEST).addClassIn(AboraSupport.findAboraClass(ImmuSetTester.class).getOrMakeCxxClassDescription(), TEST).addClassIn(AboraSupport.findAboraClass(SHTO.class).getOrMakeCxxClassDescription(), TEST).addClassIn(AboraSupport.findAboraClass(HashSetTester.class).getOrMakeCxxClassDescription(), TEST);
	(CxxSystemOrganization.fileNamed("settab")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("settab")).addClassIn(AboraSupport.findAboraClass(SetTable.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("settab")).addClassIn(AboraSupport.findAboraClass(SetTableStepper.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.getOrMakeFileNamed("settab")).addClassIn(AboraSupport.findAboraClass(SetTableTester.class).getOrMakeCxxClassDescription(), TEST);
	(CxxSystemOrganization.fileNamed("cross")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("cross")).hxxHeaderIn("/* xpp class */\n"+
"", PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("cross")).addClassIn(AboraSupport.findAboraClass(CrossSpace.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(CrossRegion.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(CrossOrderSpec.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(CrossMapping.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(Tuple.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("cross")).addClassIn(AboraSupport.findAboraClass(ActualTuple.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(GenericCrossSimpleRegionStepper.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(GenericCrossRegion.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(GenericCrossSpace.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(BoxAccumulator.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(BoxStepper.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(TupleStepper.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(BoxProjectionStepper.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(GenericCrossDsp.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.getOrMakeFileNamed("cross")).addClassIn(AboraSupport.findAboraClass(CrossTester.class).getOrMakeCxxClassDescription(), TEST);
	(CxxSystemOrganization.fileNamed("edge")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("edge")).addClassIn(AboraSupport.findAboraClass(EdgeManager.class).getOrMakeCxxClassDescription(), PROTECTED).addClassIn(AboraSupport.findAboraClass(TransitionEdge.class).getOrMakeCxxClassDescription(), PROTECTED);
	(CxxSystemOrganization.getOrMakeFileNamed("edge")).addClassIn(AboraSupport.findAboraClass(EdgeSimpleRegionStepper.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(EdgeStepper.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(EdgeAccumulator.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.fileNamed("filter")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("filter")).hxxHeaderIn("/* xpp class */", PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("filter")).addClassIn(AboraSupport.findAboraClass(Filter.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(Joint.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(FilterPosition.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(FilterSpace.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(RegionDelta.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("filter")).addClassIn(AboraSupport.findAboraClass(AndFilter.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(SubsetFilter.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(OpenFilter.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(OrFilter.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(FilterDsp.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(NotSupersetFilter.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(SupersetFilter.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(ClosedFilter.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(NotSubsetFilter.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.getOrMakeFileNamed("filter")).addClassIn(AboraSupport.findAboraClass(FilterTester.class).getOrMakeCxxClassDescription(), TEST);
	(CxxSystemOrganization.fileNamed("hspace")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("hspace")).addClassIn(AboraSupport.findAboraClass(UnOrdered.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(HeaperSpace.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(HeaperAsPosition.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("hspace")).cxxHeaderIn("#include \"choosex.hxx\"", PRIVATE);
	(CxxSystemOrganization.getOrMakeFileNamed("hspace")).addClassIn(AboraSupport.findAboraClass(HeaperDsp.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(SetRegion.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(HeaperRegion.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(StrongAsPosition.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.fileNamed("integer")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("integer")).hxxHeaderIn("#define Integer0 IntegerPos::make(0)\n"+
"", PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("integer")).addClassIn(AboraSupport.findAboraClass(IntegerRegion.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(IntegerSpace.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(IntegerMapping.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(IntegerPos.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("integer")).hxxHeaderIn("", PRIVATE);
	(CxxSystemOrganization.getOrMakeFileNamed("integer")).addClassIn(AboraSupport.findAboraClass(IntegerArrangement.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(DescendingIntegerStepper.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(IntegerUpOrder.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(IntegerEdgeStepper.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(IntegerEdgeAccumulator.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(IntegerSimpleRegionStepper.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(AscendingIntegerStepper.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.getOrMakeFileNamed("integer")).addClassIn(AboraSupport.findAboraClass(IntegerRegionTester.class).getOrMakeCxxClassDescription(), TEST);
	(CxxSystemOrganization.fileNamed("real")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("real")).hxxHeaderIn("#define IEEE8 Int8\n"+
"", PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("real")).addClassIn(AboraSupport.findAboraClass(RealRegion.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(RealSpace.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(RealPos.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("real")).addClassIn(AboraSupport.findAboraClass(RealDsp.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(RealUpOrder.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(RealManager.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(RealEdge.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(RealStepper.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(IEEE32Pos.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(BeforeReal.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(IEEE64Pos.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(AfterReal.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(IEEE8Pos.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.getOrMakeFileNamed("real")).addClassIn(AboraSupport.findAboraClass(RealTester.class).getOrMakeCxxClassDescription(), TEST);
	(CxxSystemOrganization.fileNamed("sequenc")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("sequenc")).cxxHeaderIn("#include <sys/types.h>\n"+
"#ifndef WIN32\n"+
"#	include <sys/time.h>\n"+
"#else\n"+
"#	include <sys/timeb.h>\n"+
"#endif /* WIN32 */\n"+
"", PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("sequenc")).addClassIn(AboraSupport.findAboraClass(SequenceMapping.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(SequenceRegion.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(Sequence.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(SequenceSpace.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("sequenc")).addClassIn(AboraSupport.findAboraClass(SequenceEdge.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(SequenceManager.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(BeforeSequencePrefix.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(SequenceStepper.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(SequenceUpOrder.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(AfterSequence.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(BeforeSequence.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.getOrMakeFileNamed("sequenc")).addClassIn(AboraSupport.findAboraClass(SequenceTester.class).getOrMakeCxxClassDescription(), TEST);
	(CxxSystemOrganization.fileNamed("space")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("space")).hxxHeaderIn("/* xpp class */\n"+
"\n"+
"typedef enum { LESS_THAN, EQUAL, GREATER_THAN, INCOMPARABLE }\n"+
"	OrderEnum;", PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("space")).addClassIn(AboraSupport.findAboraClass(Mapping.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(Arrangement.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(CoordinateSpace.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(OrderSpec.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(XnRegion.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(Dsp.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(Position.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("space")).addClassIn(AboraSupport.findAboraClass(MergeStepper.class).getOrMakeCxxClassDescription(), PROTECTED).addClassIn(AboraSupport.findAboraClass(ExplicitArrangement.class).getOrMakeCxxClassDescription(), PROTECTED).addClassIn(AboraSupport.findAboraClass(IdentityDsp.class).getOrMakeCxxClassDescription(), PROTECTED).addClassIn(AboraSupport.findAboraClass(BasicSpace.class).getOrMakeCxxClassDescription(), PROTECTED);
	(CxxSystemOrganization.getOrMakeFileNamed("space")).addClassIn(AboraSupport.findAboraClass(DisjointRegionStepper.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(ConstantMapping.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(SimpleMapping.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(ReverseOrder.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(EmptyMapping.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(CompositeMapping.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.getOrMakeFileNamed("space")).addClassIn(AboraSupport.findAboraClass(RegionTester.class).getOrMakeCxxClassDescription(), TEST);
	(CxxSystemOrganization.fileNamed("stepper")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("stepper")).hxxHeaderIn("#ifdef MANUAL_CRUTCH\n"+
"#	define BEGIN_FOR_EACH(TYPE,VAR,EXPR) {						\\\n"+
"		SPTR(Stepper) OF(TYPE) loop_Stepper = (EXPR);			\\\n"+
"		SPTR(TYPE) VAR;												\\\n"+
"		while ((VAR = CAST(TYPE,loop_Stepper->fetch())) != NULL ) {\n"+
"\n"+
"\n"+
"#define END_FOR_EACH											\\\n"+
"			loop_Stepper->step();									\\\n"+
"		}															\\\n"+
"		loop_Stepper->destroy();									\\\n"+
"	}\n"+
"#else /* MANUAL_CRUTCH */\n"+
"\n"+
"#	define BEGIN_FOR_EACH(TYPE,VAR,EXPR) {					\\\n"+
"		SPTR(TYPE) VAR;											\\\n"+
"		for(SPTR(Stepper) OF(TYPE) loop_Stepper = (EXPR);		\\\n"+
"				loop_Stepper->hasValue();							\\\n"+
"				loop_Stepper->step()) {							\\\n"+
"			VAR = CAST(TYPE,loop_Stepper->fetch());	\n"+
"\n"+
"\n"+
"#define END_FOR_EACH											\\\n"+
"		}															\\\n"+
"		loop_Stepper->destroy();									\\\n"+
"	}\n"+
"#endif /* MANUAL_CRUTCH */\n"+
"\n"+
"#ifdef MANUAL_CRUTCH\n"+
"\n"+
"#	define BEGIN_FOR_POSITIONS(TYPE1,POSITION,TYPE2,VALUE,EXPR) {						\\\n"+
"		SPTR(TableStepper) OF(TYPE1) loop_Stepper = CAST(TableStepper,(EXPR));		\\\n"+
"		SPTR(TYPE1) POSITION;											\\\n"+
"		SPTR(TYPE2) VALUE;										\\\n"+
"		while ( loop_Stepper->hasValue() ) {						\\\n"+
"			POSITION = CAST(TYPE1,loop_Stepper->position());				\\\n"+
"			VALUE = CAST(TYPE2,loop_Stepper->fetch());	\n"+
"			\n"+
"#define END_FOR_POSITIONS				\\\n"+
"			loop_Stepper->step();				\\\n"+
"		}										\\\n"+
"		loop_Stepper->destroy();				\\\n"+
"	}\n"+
"\n"+
"#	define BEGIN_FOR_INDICES(INDEX,TYPE2,VALUE,EXPR) {						\\\n"+
"		SPTR(TableStepper) OF(TYPE1) loop_Stepper = CAST(TableStepper,(EXPR));		\\\n"+
"		IntegerVar INDEX;											\\\n"+
"		SPTR(TYPE2) VALUE;										\\\n"+
"		while ( loop_Stepper->hasValue() ) {						\\\n"+
"			INDEX = loop_Stepper->index();				\\\n"+
"			VALUE = CAST(TYPE2,loop_Stepper->fetch());	\n"+
"			\n"+
"#define END_FOR_INDICES				\\\n"+
"			loop_Stepper->step();				\\\n"+
"		}										\\\n"+
"		loop_Stepper->destroy();				\\\n"+
"	}\n"+
"\n"+
"#else /* MANUAL_CRUTCH */\n"+
"\n"+
"#	define BEGIN_FOR_POSITIONS(TYPE1,POSITION,TYPE2,VALUE,EXPR) {			\\\n"+
"		SPTR(TYPE1) POSITION;											\\\n"+
"		SPTR(TYPE2) VALUE;										\\\n"+
"		for(	 SPTR(TableStepper) OF(TYPE1) loop_Stepper = CAST(TableStepper,(EXPR));		\\\n"+
"				loop_Stepper->hasValue();							\\\n"+
"				loop_Stepper->step()) {							\\\n"+
"			POSITION = CAST(TYPE1,loop_Stepper->position());				\\\n"+
"			VALUE = CAST(TYPE2,loop_Stepper->fetch());\n"+
"\n"+
"#define END_FOR_POSITIONS				\\\n"+
"		}										\\\n"+
"		loop_Stepper->destroy();				\\\n"+
"	}\n"+
"\n"+
"#	define BEGIN_FOR_INDICES(INDEX,TYPE2,VALUE,EXPR) {			\\\n"+
"		IntegerVar INDEX;											\\\n"+
"		SPTR(TYPE2) VALUE;										\\\n"+
"		for(	 SPTR(TableStepper) OF(TYPE1) loop_Stepper = CAST(TableStepper,(EXPR));		\\\n"+
"				loop_Stepper->hasValue();							\\\n"+
"				loop_Stepper->step()) {							\\\n"+
"			INDEX = loop_Stepper->index();				\\\n"+
"			VALUE = CAST(TYPE2,loop_Stepper->fetch());\n"+
"\n"+
"#define END_FOR_INDICES				\\\n"+
"		}										\\\n"+
"		loop_Stepper->destroy();				\\\n"+
"	}\n"+
"\n"+
"#endif /* MANUAL_CRUTCH */\n"+
"", PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("stepper")).addClassIn(AboraSupport.findAboraClass(Stepper.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(Accumulator.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(TableStepper.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("stepper")).addClassIn(AboraSupport.findAboraClass(EmptyStepper.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(PtrArrayStepper.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(PtrArrayAccumulator.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(ItemStepper.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.fileNamed("array")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("array")).hxxHeaderIn("/* xpp class */\n"+
"", PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("array")).cxxHeaderIn("#include <string.h>\n"+
"#include <stream.h>\n"+
"", PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("array")).addClassIn(AboraSupport.findAboraClass(MuArray.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("array")).hxxHeaderIn("", PRIVATE);
	(CxxSystemOrganization.getOrMakeFileNamed("array")).addClassIn(AboraSupport.findAboraClass(OffsetScruArray.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(ArrayStepper.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(AscendingArrayStepper.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(ArrayAccumulator.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(OffsetArrayStepper.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(ActualArray.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.fileNamed("hashtab")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("hashtab")).hxxHeaderIn("", PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("hashtab")).cxxHeaderIn("#include \"integerx.hxx\"", PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("hashtab")).addClassIn(AboraSupport.findAboraClass(HashTable.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("hashtab")).addClassIn(AboraSupport.findAboraClass(ActualHashTable.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.getOrMakeFileNamed("hashtab")).hxxHeaderIn("\n"+
"#include <stream.h>", TEST);
	(CxxSystemOrganization.getOrMakeFileNamed("hashtab")).cxxHeaderIn("#include \"integerx.hxx\" /* missed by include scanner */", TEST);
	(CxxSystemOrganization.getOrMakeFileNamed("hashtab")).addClassIn(AboraSupport.findAboraClass(HashTableTester.class).getOrMakeCxxClassDescription(), TEST);
	(CxxSystemOrganization.fileNamed("inttab")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("inttab")).commentIn("This file represents the tables with Integer domains.  It provides the abstract\n"+
"classes IntegerTable and its (abstract) subclass Array.  IntegerTable may be used for\n"+
"any Integer keys, while Array is restricted to be zero based, and to have contiguous\n"+
"Integer keys.", PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("inttab")).addClassIn(AboraSupport.findAboraClass(IntegerTable.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("inttab")).addClassIn(AboraSupport.findAboraClass(IntegerTableStepper.class).getOrMakeCxxClassDescription(), PROTECTED);
	(CxxSystemOrganization.getOrMakeFileNamed("inttab")).hxxHeaderIn("", PRIVATE);
	(CxxSystemOrganization.getOrMakeFileNamed("inttab")).addClassIn(AboraSupport.findAboraClass(ITAscendingStepper.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(OberIntegerTable.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(ActualIntegerTable.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(COWIntegerTable.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(ITDescendingStepper.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(ITGenericStepper.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.getOrMakeFileNamed("inttab")).hxxHeaderIn("#include <stream.h>", TEST);
	(CxxSystemOrganization.getOrMakeFileNamed("inttab")).addClassIn(AboraSupport.findAboraClass(IntegerTableTester.class).getOrMakeCxxClassDescription(), TEST);
	(CxxSystemOrganization.fileNamed("tabent")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("tabent")).addClassIn(AboraSupport.findAboraClass(TableEntry.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("tabent")).addClassIn(AboraSupport.findAboraClass(IndexEntry.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(PositionEntry.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(BucketArrayStepper.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(HeaperAsEntry.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(HashIndexEntry.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.getOrMakeFileNamed("tabent")).cxxHeaderIn("#include \"stepperx.hxx\"", TEST);
	(CxxSystemOrganization.getOrMakeFileNamed("tabent")).addClassIn(AboraSupport.findAboraClass(TableEntryTester.class).getOrMakeCxxClassDescription(), TEST);
	(CxxSystemOrganization.fileNamed("tables")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("tables")).commentIn("This file declares the Table classes.  These are the basic X++ collection\n"+
"classes.  They are designed to be purely collections, with no excess\n"+
"protocol for ordering or enumeration.  A table is considered to be a collection\n"+
"which maps from Positions to arbitrary objects (Heapers).  They are\n"+
"defined as having a coordinate space for the domain.\n"+
"\n"+
"mumble mumble, more explanation later.", PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("tables")).addClassIn(AboraSupport.findAboraClass(ScruTable.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(MuTable.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(ImmuTable.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(Pair.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(TableAccumulator.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("tables")).cxxHeaderIn("#include \"choosex.hxx\"\n"+
"", PRIVATE);
	(CxxSystemOrganization.getOrMakeFileNamed("tables")).addClassIn(AboraSupport.findAboraClass(OffsetImmuTable.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(OffsetScruTableStepper.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(IntegerScruTable.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(ImmuTableOnMu.class).getOrMakeCxxClassDescription(), PRIVATE).addClassIn(AboraSupport.findAboraClass(OffsetScruTable.class).getOrMakeCxxClassDescription(), PRIVATE);
	(CxxSystemOrganization.fileNamed("tabtool")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("tabtool")).addClassIn(AboraSupport.findAboraClass(PrimeSizeProvider.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(LPPrimeSizeProvider.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.fileNamed("tester")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("tester")).hxxHeaderIn("#include <string.h>", PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("tester")).addClassIn(AboraSupport.findAboraClass(Tester.class).getOrMakeCxxClassDescription(), PUBLIC);
	(CxxSystemOrganization.getOrMakeFileNamed("tester")).hxxHeaderIn("#include <string.h>\n"+
"#include <stream.h>", TEST);
	(CxxSystemOrganization.getOrMakeFileNamed("tester")).addClassIn(AboraSupport.findAboraClass(HelloTester.class).getOrMakeCxxClassDescription(), TEST);
	(CxxSystemOrganization.fileNamed("thunk")).comment("");
	(CxxSystemOrganization.getOrMakeFileNamed("thunk")).addClassIn(AboraSupport.findAboraClass(Thunk.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(EchoThunk.class).getOrMakeCxxClassDescription(), PUBLIC).addClassIn(AboraSupport.findAboraClass(CommentThunk.class).getOrMakeCxxClassDescription(), PUBLIC);
	CxxSystemOrganization.tree((CxxSystemOrganization.tree().addChild(((CxxTreeAssociation.keyValue("top", null)).addChild(((CxxTreeAssociation.keyValue("xhs", null)).addChild(((CxxTreeAssociation.keyValue("comm", DIR)).addChild(((CxxTreeAssociation.keyValue("bootpln", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("comdtct", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("handlrs", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("memstr", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("negoti8", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("nscotty", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("portal", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("prolstn", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("proman", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("servers", null)).addChild(((CxxTreeAssociation.keyValue("schunk", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("sktsrv", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("srvloop", FILE)).yourself())).yourself())).addChild(((CxxTreeAssociation.keyValue("transfers", null)).addChild(((CxxTreeAssociation.keyValue("bin2com", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("cookbk", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("nxcvr", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("recipe", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("txtcomm", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("xfrspec", FILE)).yourself())).yourself())).yourself())).addChild(((CxxTreeAssociation.keyValue("disk", DIR)).addChild(((CxxTreeAssociation.keyValue("consist", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("diskman", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("dstat", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("fakedsk", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("flkinfo", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("packer", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("purging", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("snfinfo", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("turtle", FILE)).yourself())).yourself())).addChild(((CxxTreeAssociation.keyValue("server", DIR)).addChild(((CxxTreeAssociation.keyValue("nfebe", null)).addChild(((CxxTreeAssociation.keyValue("brange1", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("brange2", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("brange3", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("crypto", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("detect", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("granmap", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("id", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("nadmin", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("nkernel", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("nlinks", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("sysadm", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("wrapper", FILE)).yourself())).yourself())).addChild(((CxxTreeAssociation.keyValue("main", null)).addChild(((CxxTreeAssociation.keyValue("sheph", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("tclude", FILE)).yourself())).yourself())).addChild(((CxxTreeAssociation.keyValue("ent", null)).addChild(((CxxTreeAssociation.keyValue("ent", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("htree", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("loaves", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("oroot", FILE)).yourself())).yourself())).addChild(((CxxTreeAssociation.keyValue("tracedag", null)).addChild(((CxxTreeAssociation.keyValue("branch", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("dagwood", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("tracep", FILE)).yourself())).yourself())).addChild(((CxxTreeAssociation.keyValue("entcanopy", null)).addChild(((CxxTreeAssociation.keyValue("canopy", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("props", FILE)).yourself())).yourself())).addChild(((CxxTreeAssociation.keyValue("serverxpp", null)).addChild(((CxxTreeAssociation.keyValue("counter", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("grantab", FILE)).yourself())).yourself())).addChild(((CxxTreeAssociation.keyValue("forfebe", null)).addChild(((CxxTreeAssociation.keyValue("worksrv", FILE)).yourself())).yourself())).yourself())).addChild(((CxxTreeAssociation.keyValue("xlatexpp", DIR)).addChild(((CxxTreeAssociation.keyValue("lowlevel", null)).addChild(((CxxTreeAssociation.keyValue("become", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("cache", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("gchooks", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("hlogger", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("tokens", FILE)).yourself())).yourself())).addChild(((CxxTreeAssociation.keyValue("primtab", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("primval", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("rcmain", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("set", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("settab", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("spaces", null)).addChild(((CxxTreeAssociation.keyValue("cross", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("edge", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("filter", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("hspace", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("integer", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("real", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("sequenc", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("space", FILE)).yourself())).yourself())).addChild(((CxxTreeAssociation.keyValue("stepper", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("tabless", null)).addChild(((CxxTreeAssociation.keyValue("array", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("hashtab", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("inttab", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("tabent", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("tables", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("tabtool", FILE)).yourself())).yourself())).addChild(((CxxTreeAssociation.keyValue("tester", FILE)).yourself())).addChild(((CxxTreeAssociation.keyValue("thunk", FILE)).yourself())).yourself())).yourself())).yourself()))));
/*

Generated during transformation: AddMethod
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print("(");
	oo.print(myNext);
	if (myBuffer == null) {
		oo.print(")");
		return ;
	}
	oo.print(", ");
	oo.print(myBuffer.count());
	oo.print(", \"");
	for (int i = 0; i < myNext; i ++ ) {
		oo.print(((myBuffer.uIntAt(i))));
	}
	oo.print("<-|->");
	for (int j = myNext; j < myBuffer.count(); j ++ ) {
		oo.print(((myBuffer.uIntAt(j))));
	}
	oo.print("\")");
/*
udanax-top.st:70647:XnBufferedWriteStream methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	
	oo << self getCategory name << '(' << myNext.
	myBuffer == NULL ifTrue: [oo << ')'.  ^VOID].
	oo << ', ' << myBuffer count << ', "'.
	Int32Zero almostTo: myNext do: [:i {Int32} | oo DOTput: ((myBuffer uIntAt: i) basicCast: Character)].
	oo << '<-|->'.
	myNext almostTo: myBuffer count do: [:j {Int32} | oo DOTput: ((myBuffer uIntAt: j) basicCast: Character)].
	oo << '")'!
*/
}
public void writePacket() {
	myPortal.writePacket(myBuffer, myNext);
	myNext = 0;
/*
udanax-top.st:70659:XnBufferedWriteStream methodsFor: 'private'!
{void} writePacket
	myPortal writePacket: myBuffer with: myNext.
	myNext _ Int32Zero.!
*/
}
public void flush() {
	writePacket();
	myPortal.flush();
/*
udanax-top.st:70666:XnBufferedWriteStream methodsFor: 'accessing'!
{void} flush
	self writePacket.
	myPortal flush!
*/
}
public void putByte(int bytex) {
	if (myBuffer == null) {
		myBuffer = myPortal.writeBuffer();
	}
	myBuffer.storeUInt(myNext, bytex);
	myNext = myNext + 1;
	if (myNext >= myBuffer.count()) {
		writePacket();
	}
/*
udanax-top.st:70671:XnBufferedWriteStream methodsFor: 'accessing'!
{void} putByte: byte {UInt32} 
	myBuffer == NULL ifTrue: [myBuffer _ myPortal writeBuffer].
	myBuffer at: myNext storeUInt: byte.
	myNext _ myNext + 1.
	myNext >= myBuffer count ifTrue: [self writePacket]!
*/
}
/**
 * This should be optimized to use a memcpy or something rather than a loop
 */
public void putData(UInt8Array array) {
	int size;
	size = array.count();
	Someone.thingToDo();
	for (int i = 
	/* Make this use primitives. */
	0; i <= size-1; i ++ ) {
		putByte((array.uIntAt(i)));
	}
/*
udanax-top.st:70677:XnBufferedWriteStream methodsFor: 'accessing'!
{void} putData: array {UInt8Array}
	"This should be optimized to use a memcpy or something rather than a loop"
	
	| size {UInt32} |
	size _ array count.
	self thingToDo.  "Make this use primitives."
	Int32Zero to: size -1 do: [:i {Int32} | self putByte: (array uIntAt: i)].!
*/
}
public void putStr(String string) {
	/* This should be optimized to use a memcpy or something rather than a loop */
	AboraSupport.smalltalkOnly();
	{
		for (int doIndex = 0; doIndex < string.length(); doIndex ++ ) {
			char ch = string.charAt(doIndex);
			putByte(ch);
		}
	}
	AboraSupport.translateOnly();
	{
		/* 
	while (*string) {
		this->putByte(*string++);
	} */
	}
/*
udanax-top.st:70685:XnBufferedWriteStream methodsFor: 'accessing'!
{void} putStr: string {char star}
	""
	"This should be optimized to use a memcpy or something rather than a loop"
	
	[string do: [:ch {Character} | self putByte: ch uint8]] smalltalkOnly.
	
	'
	while (*string) {
		this->putByte(*string++);
	}' translateOnly!
*/
}
public XnBufferedWriteStream(PacketPortal portal) {
	super();
	myPortal = portal;
	myBuffer = null;
	myNext = 0;
/*
udanax-top.st:70698:XnBufferedWriteStream methodsFor: 'creation'!
create: portal {PacketPortal}
	super create.
	myPortal _ portal.
	myBuffer _ NULL.
	myNext _ Int32Zero.!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:70707:XnBufferedWriteStream methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:70709:XnBufferedWriteStream methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public XnBufferedWriteStream() {
/*

Generated during transformation
*/
}
public XnBufferedWriteStream(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
