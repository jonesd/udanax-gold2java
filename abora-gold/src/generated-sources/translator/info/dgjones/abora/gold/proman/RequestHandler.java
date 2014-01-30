/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.proman;

import info.dgjones.abora.gold.be.basic.ID;
import info.dgjones.abora.gold.be.locks.BooLock;
import info.dgjones.abora.gold.be.locks.ChallengeLock;
import info.dgjones.abora.gold.be.locks.Lock;
import info.dgjones.abora.gold.be.locks.MatchLock;
import info.dgjones.abora.gold.be.locks.MultiLock;
import info.dgjones.abora.gold.collection.basic.IntegerVarArray;
import info.dgjones.abora.gold.collection.basic.PrimArray;
import info.dgjones.abora.gold.collection.basic.PrimFloatArray;
import info.dgjones.abora.gold.collection.basic.PrimIntArray;
import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.collection.steppers.TableStepper;
import info.dgjones.abora.gold.cross.CrossMapping;
import info.dgjones.abora.gold.cross.CrossOrderSpec;
import info.dgjones.abora.gold.filter.Filter;
import info.dgjones.abora.gold.filter.FilterPosition;
import info.dgjones.abora.gold.filter.FilterSpace;
import info.dgjones.abora.gold.id.IDRegion;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.Fn;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nadmin.FeBooLockSmith;
import info.dgjones.abora.gold.nadmin.FeChallengeLockSmith;
import info.dgjones.abora.gold.nadmin.FeClubDescription;
import info.dgjones.abora.gold.nadmin.FeLockSmith;
import info.dgjones.abora.gold.nadmin.FeMatchLockSmith;
import info.dgjones.abora.gold.nadmin.FeMultiLockSmith;
import info.dgjones.abora.gold.nadmin.FeSession;
import info.dgjones.abora.gold.nadmin.FeWallLockSmith;
import info.dgjones.abora.gold.nkernel.FeAdminer;
import info.dgjones.abora.gold.nkernel.FeArrayBundle;
import info.dgjones.abora.gold.nkernel.FeBundle;
import info.dgjones.abora.gold.nkernel.FeClub;
import info.dgjones.abora.gold.nkernel.FeDataHolder;
import info.dgjones.abora.gold.nkernel.FeEdition;
import info.dgjones.abora.gold.nkernel.FeElementBundle;
import info.dgjones.abora.gold.nkernel.FeIDHolder;
import info.dgjones.abora.gold.nkernel.FeKeyMaster;
import info.dgjones.abora.gold.nkernel.FeLabel;
import info.dgjones.abora.gold.nkernel.FeRangeElement;
import info.dgjones.abora.gold.nkernel.FeServer;
import info.dgjones.abora.gold.nkernel.FeWork;
import info.dgjones.abora.gold.nlinks.FeHyperLink;
import info.dgjones.abora.gold.nlinks.FeHyperRef;
import info.dgjones.abora.gold.nlinks.FeMultiRef;
import info.dgjones.abora.gold.nlinks.FePath;
import info.dgjones.abora.gold.nlinks.FeSingleRef;
import info.dgjones.abora.gold.proman.PromiseManager;
import info.dgjones.abora.gold.proman.RequestHandler;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.Mapping;
import info.dgjones.abora.gold.spaces.basic.OrderSpec;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.cross.CrossRegion;
import info.dgjones.abora.gold.spaces.cross.CrossSpace;
import info.dgjones.abora.gold.spaces.cross.Tuple;
import info.dgjones.abora.gold.spaces.integers.IntegerMapping;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.spaces.integers.IntegerRegion;
import info.dgjones.abora.gold.spaces.integers.IntegerSpace;
import info.dgjones.abora.gold.spaces.unordered.IDSpace;
import info.dgjones.abora.gold.sysadm.FeArchiver;
import info.dgjones.abora.gold.tumbler.RealPos;
import info.dgjones.abora.gold.tumbler.RealRegion;
import info.dgjones.abora.gold.tumbler.RealSpace;
import info.dgjones.abora.gold.tumbler.Sequence;
import info.dgjones.abora.gold.tumbler.SequenceMapping;
import info.dgjones.abora.gold.tumbler.SequenceRegion;
import info.dgjones.abora.gold.tumbler.SequenceSpace;
import info.dgjones.abora.gold.wrapper.FeSet;
import info.dgjones.abora.gold.wrapper.FeText;
import info.dgjones.abora.gold.wrapper.FeWrapper;
import info.dgjones.abora.gold.wrapper.FeWrapperSpec;
import info.dgjones.abora.gold.x.PrimFloatValue;
import info.dgjones.abora.gold.x.PrimIntValue;
import info.dgjones.abora.gold.x.PrimValue;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

/**
 * A class for each abstract signature.  Each instance will wrap a pointer to a static member
 * function.
 */
public class RequestHandler extends Heaper {

/*
udanax-top.st:42217:
Heaper subclass: #RequestHandler
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-proman'!
*/
/*
udanax-top.st:42221:
RequestHandler comment:
'A class for each abstract signature.  Each instance will wrap a pointer to a static member function.'!
*/
/*
udanax-top.st:42223:
(RequestHandler getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
/*
udanax-top.st:42245:
RequestHandler class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:42248:
(RequestHandler getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(RequestHandler.class).setAttributes( new Set().add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
public void handleRequest(PromiseManager pm) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:42228:RequestHandler methodsFor: 'request handling'!
{void} handleRequest: pm {PromiseManager}
	self subclassResponsibility!
*/
}
public void printOn(PrintWriter oo) {
	Fn staticFn;
	staticFn = instVarAt(1);
	oo.print(staticFn.staticClass());
	oo.print("::");
	oo.print(staticFn.selector());
/*
udanax-top.st:42234:RequestHandler methodsFor: 'smalltalk: printing'!
printOn: oo
	| staticFn |
	staticFn _ self instVarAt: 1.
	oo << staticFn staticClass << '::' << staticFn selector!
*/
}
public int actualHashForEqual() {
	return Heaper.takeOop();
/*
udanax-top.st:42241:RequestHandler methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^Heaper takeOop!
*/
}
public static void AdminerUacceptConnectionsUN2(FeAdminer receiver, boolean arg1) {
	receiver.acceptConnections(arg1);
/*
udanax-top.st:42253:RequestHandler class methodsFor: 'translate: generated'!
{void} Adminer.U.acceptConnections.U.N2: receiver {FeAdminer} with: arg1 {BooleanVar}
	receiver acceptConnections: arg1!
*/
}
public static Stepper AdminerUactiveSessionsUN1(FeAdminer receiver) {
	return receiver.activeSessions();
/*
udanax-top.st:42256:RequestHandler class methodsFor: 'translate: generated'!
{Stepper} Adminer.U.activeSessions.U.N1: receiver {FeAdminer}
	^receiver activeSessions!
*/
}
public static void AdminerUexecuteUN2(FeAdminer receiver, PrimIntArray arg1) {
	receiver.execute(arg1);
/*
udanax-top.st:42259:RequestHandler class methodsFor: 'translate: generated'!
{void} Adminer.U.execute.U.N2: receiver {FeAdminer} with: arg1 {PrimIntArray}
	receiver execute: arg1!
*/
}
public static FeLockSmith AdminerUgateLockSmithUN1(FeAdminer receiver) {
	return receiver.gateLockSmith();
/*
udanax-top.st:42262:RequestHandler class methodsFor: 'translate: generated'!
{FeLockSmith} Adminer.U.gateLockSmith.U.N1: receiver {FeAdminer}
	^receiver gateLockSmith!
*/
}
public static void AdminerUgrantUN3(FeAdminer receiver, ID arg1, IDRegion arg2) {
	receiver.grant(arg1, arg2);
/*
udanax-top.st:42265:RequestHandler class methodsFor: 'translate: generated'!
{void} Adminer.U.grant.U.N3: receiver {FeAdminer} with: arg1 {ID} with: arg2 {IDRegion}
	receiver grant: arg1 with: arg2!
*/
}
public static TableStepper AdminerUgrantsUN1(FeAdminer receiver) {
	return receiver.grants();
/*
udanax-top.st:42268:RequestHandler class methodsFor: 'translate: generated'!
{TableStepper} Adminer.U.grants.U.N1: receiver {FeAdminer}
	^receiver grants!
*/
}
public static TableStepper AdminerUgrantsUN2(FeAdminer receiver, IDRegion arg1) {
	return receiver.grants(arg1);
/*
udanax-top.st:42271:RequestHandler class methodsFor: 'translate: generated'!
{TableStepper} Adminer.U.grants.U.N2: receiver {FeAdminer} with: arg1 {IDRegion}
	^receiver grants: arg1!
*/
}
public static TableStepper AdminerUgrantsUN3(FeAdminer receiver, IDRegion arg1, IDRegion arg2) {
	return receiver.grants(arg1, arg2);
/*
udanax-top.st:42274:RequestHandler class methodsFor: 'translate: generated'!
{TableStepper} Adminer.U.grants.U.N3: receiver {FeAdminer} with: arg1 {IDRegion} with: arg2 {IDRegion}
	^receiver grants: arg1 with: arg2!
*/
}
public static boolean AdminerUisAcceptingConnectionsUN1(FeAdminer receiver) {
	return receiver.isAcceptingConnections();
/*
udanax-top.st:42277:RequestHandler class methodsFor: 'translate: generated'!
{BooleanVar} Adminer.U.isAcceptingConnections.U.N1: receiver {FeAdminer}
	^receiver isAcceptingConnections!
*/
}
public static FeAdminer AdminerUmakeUN0() {
	return FeAdminer.make();
/*
udanax-top.st:42280:RequestHandler class methodsFor: 'translate: generated'!
{FeAdminer} Adminer.U.make.U.N0
	^FeAdminer make!
*/
}
public static void AdminerUsetGateLockSmithUN2(FeAdminer receiver, FeLockSmith arg1) {
	receiver.setGateLockSmith(arg1);
/*
udanax-top.st:42283:RequestHandler class methodsFor: 'translate: generated'!
{void} Adminer.U.setGateLockSmith.U.N2: receiver {FeAdminer} with: arg1 {FeLockSmith}
	receiver setGateLockSmith: arg1!
*/
}
public static FeEdition ArchiverUarchiveUN3(FeArchiver receiver, FeEdition arg1, FeEdition arg2) {
	return receiver.archive(arg1, arg2);
/*
udanax-top.st:42286:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} Archiver.U.archive.U.N3: receiver {FeArchiver} with: arg1 {FeEdition} with: arg2 {FeEdition}
	^receiver archive: arg1 with: arg2!
*/
}
public static FeArchiver ArchiverUmakeUN0() {
	return FeArchiver.make();
/*
udanax-top.st:42289:RequestHandler class methodsFor: 'translate: generated'!
{FeArchiver} Archiver.U.make.U.N0
	^FeArchiver make!
*/
}
public static void ArchiverUmarkArchivedUN2(FeArchiver receiver, FeEdition arg1) {
	receiver.markArchived(arg1);
/*
udanax-top.st:42292:RequestHandler class methodsFor: 'translate: generated'!
{void} Archiver.U.markArchived.U.N2: receiver {FeArchiver} with: arg1 {FeEdition}
	receiver markArchived: arg1!
*/
}
public static FeEdition ArchiverUrestoreUN3(FeArchiver receiver, FeEdition arg1, FeEdition arg2) {
	return receiver.restore(arg1, arg2);
/*
udanax-top.st:42295:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} Archiver.U.restore.U.N3: receiver {FeArchiver} with: arg1 {FeEdition} with: arg2 {FeEdition}
	^receiver restore: arg1 with: arg2!
*/
}
public static PrimArray ArrayUcopyUN1(PrimArray receiver) {
	return receiver.copy();
/*
udanax-top.st:42298:RequestHandler class methodsFor: 'translate: generated'!
{PrimArray} Array.U.copy.U.N1: receiver {PrimArray}
	^receiver copy!
*/
}
public static PrimArray ArrayUcopyUN2(PrimArray receiver, PrimIntValue arg1) {
	return receiver.copy(arg1.asInt32());
/*
udanax-top.st:42301:RequestHandler class methodsFor: 'translate: generated'!
{PrimArray} Array.U.copy.U.N2: receiver {PrimArray} with: arg1 {PrimIntValue}
	^receiver copy: arg1 asInt32!
*/
}
public static PrimArray ArrayUcopyUN3(PrimArray receiver, PrimIntValue arg1, PrimIntValue arg2) {
	return receiver.copy(arg1.asInt32(), arg2.asInt32());
/*
udanax-top.st:42304:RequestHandler class methodsFor: 'translate: generated'!
{PrimArray} Array.U.copy.U.N3: receiver {PrimArray} with: arg1 {PrimIntValue} with: arg2 {PrimIntValue}
	^receiver copy: arg1 asInt32 with: arg2 asInt32!
*/
}
public static PrimArray ArrayUcopyUN4(PrimArray receiver, PrimIntValue arg1, PrimIntValue arg2, PrimIntValue arg3) {
	return receiver.copy(arg1.asInt32(), arg2.asInt32(), arg3.asInt32());
/*
udanax-top.st:42307:RequestHandler class methodsFor: 'translate: generated'!
{PrimArray} Array.U.copy.U.N4: receiver {PrimArray} with: arg1 {PrimIntValue} with: arg2 {PrimIntValue} with: arg3 {PrimIntValue}
	^receiver copy: arg1 asInt32 with: arg2 asInt32 with: arg3 asInt32!
*/
}
public static PrimArray ArrayUcopyUN5(PrimArray receiver, PrimIntValue arg1, PrimIntValue arg2, PrimIntValue arg3, PrimIntValue arg4) {
	return receiver.copy(arg1.asInt32(), arg2.asInt32(), arg3.asInt32(), arg4.asInt32());
/*
udanax-top.st:42310:RequestHandler class methodsFor: 'translate: generated'!
{PrimArray} Array.U.copy.U.N5: receiver {PrimArray} with: arg1 {PrimIntValue} with: arg2 {PrimIntValue} with: arg3 {PrimIntValue} with: arg4 {PrimIntValue}
	^receiver copy: arg1 asInt32 with: arg2 asInt32 with: arg3 asInt32 with: arg4 asInt32!
*/
}
public static PrimIntValue ArrayUcountUN1(PrimArray receiver) {
	return PrimIntValue.make((receiver.count()));
/*
udanax-top.st:42313:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntValue} Array.U.count.U.N1: receiver {PrimArray}
	^PrimIntValue make: (receiver count)!
*/
}
public static Heaper ArrayUgetUN2(PrimArray receiver, PrimIntValue arg1) {
	return receiver.getValue(arg1.asInt32());
/*
udanax-top.st:42316:RequestHandler class methodsFor: 'translate: generated'!
{Heaper} Array.U.get.U.N2: receiver {PrimArray} with: arg1 {PrimIntValue}
	^receiver getValue: arg1 asInt32!
*/
}
public static void ArrayUstoreUN3(PrimArray receiver, PrimIntValue arg1, Heaper arg2) {
	receiver.storeValue(arg1.asInt32(), arg2);
/*
udanax-top.st:42319:RequestHandler class methodsFor: 'translate: generated'!
{void} Array.U.store.U.N3: receiver {PrimArray} with: arg1 {PrimIntValue} with: arg2 {Heaper}
	receiver at: arg1 asInt32 storeValue: arg2!
*/
}
public static void ArrayUstoreAllUN1(PrimArray receiver) {
	receiver.storeAll();
/*
udanax-top.st:42322:RequestHandler class methodsFor: 'translate: generated'!
{void} Array.U.storeAll.U.N1: receiver {PrimArray}
	receiver storeAll!
*/
}
public static void ArrayUstoreAllUN2(PrimArray receiver, Heaper arg1) {
	receiver.storeAll(arg1);
/*
udanax-top.st:42325:RequestHandler class methodsFor: 'translate: generated'!
{void} Array.U.storeAll.U.N2: receiver {PrimArray} with: arg1 {Heaper}
	receiver storeAll: arg1!
*/
}
public static void ArrayUstoreAllUN3(PrimArray receiver, Heaper arg1, PrimIntValue arg2) {
	receiver.storeAll(arg1, arg2.asInt32());
/*
udanax-top.st:42328:RequestHandler class methodsFor: 'translate: generated'!
{void} Array.U.storeAll.U.N3: receiver {PrimArray} with: arg1 {Heaper} with: arg2 {PrimIntValue}
	receiver storeAll: arg1 with: arg2 asInt32!
*/
}
public static void ArrayUstoreAllUN4(PrimArray receiver, Heaper arg1, PrimIntValue arg2, PrimIntValue arg3) {
	receiver.storeAll(arg1, arg2.asInt32(), arg3.asInt32());
/*
udanax-top.st:42331:RequestHandler class methodsFor: 'translate: generated'!
{void} Array.U.storeAll.U.N4: receiver {PrimArray} with: arg1 {Heaper} with: arg2 {PrimIntValue} with: arg3 {PrimIntValue}
	receiver storeAll: arg1 with: arg2 asInt32 with: arg3 asInt32!
*/
}
public static void ArrayUstoreManyUN3(PrimArray receiver, PrimIntValue arg1, PrimArray arg2) {
	receiver.storeMany(arg1.asInt32(), arg2);
/*
udanax-top.st:42334:RequestHandler class methodsFor: 'translate: generated'!
{void} Array.U.storeMany.U.N3: receiver {PrimArray} with: arg1 {PrimIntValue} with: arg2 {PrimArray}
	receiver at: arg1 asInt32 storeMany: arg2!
*/
}
public static void ArrayUstoreManyUN4(PrimArray receiver, PrimIntValue arg1, PrimArray arg2, PrimIntValue arg3) {
	receiver.storeMany(arg1.asInt32(), arg2, arg3.asInt32());
/*
udanax-top.st:42337:RequestHandler class methodsFor: 'translate: generated'!
{void} Array.U.storeMany.U.N4: receiver {PrimArray} with: arg1 {PrimIntValue} with: arg2 {PrimArray} with: arg3 {PrimIntValue}
	receiver at: arg1 asInt32 storeMany: arg2 with: arg3 asInt32!
*/
}
public static void ArrayUstoreManyUN5(PrimArray receiver, PrimIntValue arg1, PrimArray arg2, PrimIntValue arg3, PrimIntValue arg4) {
	receiver.storeMany(arg1.asInt32(), arg2, arg3.asInt32(), arg4.asInt32());
/*
udanax-top.st:42340:RequestHandler class methodsFor: 'translate: generated'!
{void} Array.U.storeMany.U.N5: receiver {PrimArray} with: arg1 {PrimIntValue} with: arg2 {PrimArray} with: arg3 {PrimIntValue} with: arg4 {PrimIntValue}
	receiver at: arg1 asInt32 storeMany: arg2 with: arg3 asInt32 with: arg4 asInt32!
*/
}
public static PrimArray ArrayBundleUarrayUN1(FeArrayBundle receiver) {
	return receiver.array();
/*
udanax-top.st:42343:RequestHandler class methodsFor: 'translate: generated'!
{PrimArray} ArrayBundle.U.array.U.N1: receiver {FeArrayBundle}
	^receiver array!
*/
}
public static OrderSpec ArrayBundleUorderingUN1(FeArrayBundle receiver) {
	return receiver.ordering();
/*
udanax-top.st:42346:RequestHandler class methodsFor: 'translate: generated'!
{OrderSpec} ArrayBundle.U.ordering.U.N1: receiver {FeArrayBundle}
	^receiver ordering!
*/
}
public static FeKeyMaster BooLockUbooUN1(BooLock receiver) {
	return receiver.boo();
/*
udanax-top.st:42349:RequestHandler class methodsFor: 'translate: generated'!
{FeKeyMaster} BooLock.U.boo.U.N1: receiver {BooLock}
	^receiver boo!
*/
}
public static FeBooLockSmith BooLockSmithUmakeUN0() {
	return FeBooLockSmith.make();
/*
udanax-top.st:42352:RequestHandler class methodsFor: 'translate: generated'!
{FeBooLockSmith} BooLockSmith.U.make.U.N0
	^FeBooLockSmith make!
*/
}
public static XnRegion BundleUregionUN1(FeBundle receiver) {
	return receiver.region();
/*
udanax-top.st:42355:RequestHandler class methodsFor: 'translate: generated'!
{XnRegion} Bundle.U.region.U.N1: receiver {FeBundle}
	^receiver region!
*/
}
public static PrimIntArray ChallengeLockUchallengeUN1(ChallengeLock receiver) {
	return receiver.challenge();
/*
udanax-top.st:42358:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntArray} ChallengeLock.U.challenge.U.N1: receiver {ChallengeLock}
	^receiver challenge!
*/
}
public static FeKeyMaster ChallengeLockUresponseUN2(ChallengeLock receiver, PrimIntArray arg1) {
	return receiver.response(arg1);
/*
udanax-top.st:42361:RequestHandler class methodsFor: 'translate: generated'!
{FeKeyMaster} ChallengeLock.U.response.U.N2: receiver {ChallengeLock} with: arg1 {PrimIntArray}
	^receiver response: arg1!
*/
}
public static PrimIntArray ChallengeLockSmithUencrypterNameUN1(FeChallengeLockSmith receiver) {
	return receiver.encrypterName();
/*
udanax-top.st:42364:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntArray} ChallengeLockSmith.U.encrypterName.U.N1: receiver {FeChallengeLockSmith}
	^receiver encrypterName!
*/
}
public static FeChallengeLockSmith ChallengeLockSmithUmakeUN2(PrimIntArray arg1, Sequence arg2) {
	return FeChallengeLockSmith.make(arg1, arg2);
/*
udanax-top.st:42367:RequestHandler class methodsFor: 'translate: generated'!
{FeChallengeLockSmith} ChallengeLockSmith.U.make.U.N2: arg1 {PrimIntArray} with: arg2 {Sequence}
	^FeChallengeLockSmith make: arg1 with: arg2!
*/
}
public static PrimIntArray ChallengeLockSmithUpublicKeyUN1(FeChallengeLockSmith receiver) {
	return receiver.publicKey();
/*
udanax-top.st:42370:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntArray} ChallengeLockSmith.U.publicKey.U.N1: receiver {FeChallengeLockSmith}
	^receiver publicKey!
*/
}
public static FeClub ClubUmakeUN1(FeEdition arg1) {
	return (FeClub) FeClub.make(arg1);
/*
udanax-top.st:42373:RequestHandler class methodsFor: 'translate: generated'!
{FeClub} Club.U.make.U.N1: arg1 {FeEdition}
	^FeClub make: arg1!
*/
}
public static void ClubUremoveSignatureClubUN1(FeClub receiver) {
	receiver.removeSignatureClub();
/*
udanax-top.st:42376:RequestHandler class methodsFor: 'translate: generated'!
{void} Club.U.removeSignatureClub.U.N1: receiver {FeClub}
	receiver removeSignatureClub!
*/
}
public static void ClubUsetSignatureClubUN2(FeClub receiver, ID arg1) {
	receiver.setSignatureClub(arg1);
/*
udanax-top.st:42379:RequestHandler class methodsFor: 'translate: generated'!
{void} Club.U.setSignatureClub.U.N2: receiver {FeClub} with: arg1 {ID}
	receiver setSignatureClub: arg1!
*/
}
public static ID ClubUsignatureClubUN1(FeClub receiver) {
	return receiver.signatureClub();
/*
udanax-top.st:42382:RequestHandler class methodsFor: 'translate: generated'!
{ID} Club.U.signatureClub.U.N1: receiver {FeClub}
	^receiver signatureClub!
*/
}
public static FeEdition ClubUsponsoredWorksUN1(FeClub receiver) {
	return receiver.sponsoredWorks();
/*
udanax-top.st:42385:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} Club.U.sponsoredWorks.U.N1: receiver {FeClub}
	^receiver sponsoredWorks!
*/
}
public static FeEdition ClubUsponsoredWorksUN2(FeClub receiver, Filter arg1) {
	return receiver.sponsoredWorks(arg1);
/*
udanax-top.st:42388:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} Club.U.sponsoredWorks.U.N2: receiver {FeClub} with: arg1 {Filter}
	^receiver sponsoredWorks: arg1!
*/
}
public static FeLockSmith ClubDescriptionUlockSmithUN1(FeClubDescription receiver) {
	return receiver.lockSmith();
/*
udanax-top.st:42391:RequestHandler class methodsFor: 'translate: generated'!
{FeLockSmith} ClubDescription.U.lockSmith.U.N1: receiver {FeClubDescription}
	^receiver lockSmith!
*/
}
public static FeClubDescription ClubDescriptionUmakeUN2(FeSet arg1, FeLockSmith arg2) {
	return FeClubDescription.make(arg1, arg2);
/*
udanax-top.st:42394:RequestHandler class methodsFor: 'translate: generated'!
{FeClubDescription} ClubDescription.U.make.U.N2: arg1 {FeSet} with: arg2 {FeLockSmith}
	^FeClubDescription make: arg1 with: arg2!
*/
}
public static FeSet ClubDescriptionUmembershipUN1(FeClubDescription receiver) {
	return receiver.membership();
/*
udanax-top.st:42397:RequestHandler class methodsFor: 'translate: generated'!
{FeSet} ClubDescription.U.membership.U.N1: receiver {FeClubDescription}
	^receiver membership!
*/
}
public static FeClubDescription ClubDescriptionUwithLockSmithUN2(FeClubDescription receiver, FeLockSmith arg1) {
	return receiver.withLockSmith(arg1);
/*
udanax-top.st:42400:RequestHandler class methodsFor: 'translate: generated'!
{FeClubDescription} ClubDescription.U.withLockSmith.U.N2: receiver {FeClubDescription} with: arg1 {FeLockSmith}
	^receiver withLockSmith: arg1!
*/
}
public static FeClubDescription ClubDescriptionUwithMembershipUN2(FeClubDescription receiver, FeSet arg1) {
	return receiver.withMembership(arg1);
/*
udanax-top.st:42403:RequestHandler class methodsFor: 'translate: generated'!
{FeClubDescription} ClubDescription.U.withMembership.U.N2: receiver {FeClubDescription} with: arg1 {FeSet}
	^receiver withMembership: arg1!
*/
}
public static OrderSpec CoordinateSpaceUascendingUN1(CoordinateSpace receiver) {
	return receiver.ascending();
/*
udanax-top.st:42406:RequestHandler class methodsFor: 'translate: generated'!
{OrderSpec} CoordinateSpace.U.ascending.U.N1: receiver {CoordinateSpace}
	^receiver ascending!
*/
}
public static Mapping CoordinateSpaceUcompleteMappingUN2(CoordinateSpace receiver, XnRegion arg1) {
	return receiver.completeMapping(arg1);
/*
udanax-top.st:42409:RequestHandler class methodsFor: 'translate: generated'!
{Mapping} CoordinateSpace.U.completeMapping.U.N2: receiver {CoordinateSpace} with: arg1 {XnRegion}
	^receiver completeMapping: arg1!
*/
}
public static OrderSpec CoordinateSpaceUdescendingUN1(CoordinateSpace receiver) {
	return receiver.descending();
/*
udanax-top.st:42412:RequestHandler class methodsFor: 'translate: generated'!
{OrderSpec} CoordinateSpace.U.descending.U.N1: receiver {CoordinateSpace}
	^receiver descending!
*/
}
public static XnRegion CoordinateSpaceUemptyRegionUN1(CoordinateSpace receiver) {
	return receiver.emptyRegion();
/*
udanax-top.st:42415:RequestHandler class methodsFor: 'translate: generated'!
{XnRegion} CoordinateSpace.U.emptyRegion.U.N1: receiver {CoordinateSpace}
	^receiver emptyRegion!
*/
}
public static XnRegion CoordinateSpaceUfullRegionUN1(CoordinateSpace receiver) {
	return receiver.fullRegion();
/*
udanax-top.st:42418:RequestHandler class methodsFor: 'translate: generated'!
{XnRegion} CoordinateSpace.U.fullRegion.U.N1: receiver {CoordinateSpace}
	^receiver fullRegion!
*/
}
public static Mapping CoordinateSpaceUidentityMappingUN1(CoordinateSpace receiver) {
	return receiver.identityMapping();
/*
udanax-top.st:42421:RequestHandler class methodsFor: 'translate: generated'!
{Mapping} CoordinateSpace.U.identityMapping.U.N1: receiver {CoordinateSpace}
	^receiver identityMapping!
*/
}
public static Mapping CrossMappingUsubMappingUN2(CrossMapping receiver, PrimIntValue arg1) {
	return receiver.subMapping(arg1.asInt32());
/*
udanax-top.st:42424:RequestHandler class methodsFor: 'translate: generated'!
{Mapping} CrossMapping.U.subMapping.U.N2: receiver {CrossMapping} with: arg1 {PrimIntValue}
	^receiver subMapping: arg1 asInt32!
*/
}
public static PtrArray CrossMappingUsubMappingsUN1(CrossMapping receiver) {
	return receiver.subMappings();
/*
udanax-top.st:42427:RequestHandler class methodsFor: 'translate: generated'!
{PtrArray} CrossMapping.U.subMappings.U.N1: receiver {CrossMapping}
	^receiver subMappings!
*/
}
public static PrimIntArray CrossOrderSpecUlexOrderUN1(CrossOrderSpec receiver) {
	return receiver.lexOrder();
/*
udanax-top.st:42430:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntArray} CrossOrderSpec.U.lexOrder.U.N1: receiver {CrossOrderSpec}
	^receiver lexOrder!
*/
}
public static OrderSpec CrossOrderSpecUsubOrderUN2(CrossOrderSpec receiver, PrimIntValue arg1) {
	return receiver.subOrder(arg1.asInt32());
/*
udanax-top.st:42433:RequestHandler class methodsFor: 'translate: generated'!
{OrderSpec} CrossOrderSpec.U.subOrder.U.N2: receiver {CrossOrderSpec} with: arg1 {PrimIntValue}
	^receiver subOrder: arg1 asInt32!
*/
}
public static PtrArray CrossOrderSpecUsubOrdersUN1(CrossOrderSpec receiver) {
	return receiver.subOrders();
/*
udanax-top.st:42436:RequestHandler class methodsFor: 'translate: generated'!
{PtrArray} CrossOrderSpec.U.subOrders.U.N1: receiver {CrossOrderSpec}
	^receiver subOrders!
*/
}
public static Stepper CrossRegionUboxesUN1(CrossRegion receiver) {
	return receiver.boxes();
/*
udanax-top.st:42439:RequestHandler class methodsFor: 'translate: generated'!
{Stepper} CrossRegion.U.boxes.U.N1: receiver {CrossRegion}
	^receiver boxes!
*/
}
public static boolean CrossRegionUisBoxUN1(CrossRegion receiver) {
	return receiver.isBox();
/*
udanax-top.st:42442:RequestHandler class methodsFor: 'translate: generated'!
{BooleanVar} CrossRegion.U.isBox.U.N1: receiver {CrossRegion}
	^receiver isBox!
*/
}
public static XnRegion CrossRegionUprojectionUN2(CrossRegion receiver, PrimIntValue arg1) {
	return receiver.projection(arg1.asInt32());
/*
udanax-top.st:42445:RequestHandler class methodsFor: 'translate: generated'!
{XnRegion} CrossRegion.U.projection.U.N2: receiver {CrossRegion} with: arg1 {PrimIntValue}
	^receiver projection: arg1 asInt32!
*/
}
public static PtrArray CrossRegionUprojectionsUN1(CrossRegion receiver) {
	return receiver.projections();
/*
udanax-top.st:42448:RequestHandler class methodsFor: 'translate: generated'!
{PtrArray} CrossRegion.U.projections.U.N1: receiver {CrossRegion}
	^receiver projections!
*/
}
public static PtrArray CrossSpaceUaxesUN1(CrossSpace receiver) {
	return receiver.axes();
/*
udanax-top.st:42451:RequestHandler class methodsFor: 'translate: generated'!
{PtrArray} CrossSpace.U.axes.U.N1: receiver {CrossSpace}
	^receiver axes!
*/
}
public static CoordinateSpace CrossSpaceUaxisUN2(CrossSpace receiver, PrimIntValue arg1) {
	return receiver.axis(arg1.asInt32());
/*
udanax-top.st:42454:RequestHandler class methodsFor: 'translate: generated'!
{CoordinateSpace} CrossSpace.U.axis.U.N2: receiver {CrossSpace} with: arg1 {PrimIntValue}
	^receiver axis: arg1 asInt32!
*/
}
public static PrimIntValue CrossSpaceUaxisCountUN1(CrossSpace receiver) {
	return PrimIntValue.make((receiver.axisCount()));
/*
udanax-top.st:42457:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntValue} CrossSpace.U.axisCount.U.N1: receiver {CrossSpace}
	^PrimIntValue make: (receiver axisCount)!
*/
}
public static Mapping CrossSpaceUcrossOfMappingsUN1(CrossSpace receiver) {
	return receiver.crossOfMappings();
/*
udanax-top.st:42460:RequestHandler class methodsFor: 'translate: generated'!
{Mapping} CrossSpace.U.crossOfMappings.U.N1: receiver {CrossSpace}
	^receiver crossOfMappings!
*/
}
public static Mapping CrossSpaceUcrossOfMappingsUN2(CrossSpace receiver, PtrArray arg1) {
	return receiver.crossOfMappings(arg1);
/*
udanax-top.st:42463:RequestHandler class methodsFor: 'translate: generated'!
{Mapping} CrossSpace.U.crossOfMappings.U.N2: receiver {CrossSpace} with: arg1 {PtrArray}
	^receiver crossOfMappings: arg1!
*/
}
public static CrossOrderSpec CrossSpaceUcrossOfOrderSpecsUN1(CrossSpace receiver) {
	return receiver.crossOfOrderSpecs();
/*
udanax-top.st:42466:RequestHandler class methodsFor: 'translate: generated'!
{CrossOrderSpec} CrossSpace.U.crossOfOrderSpecs.U.N1: receiver {CrossSpace}
	^receiver crossOfOrderSpecs!
*/
}
public static CrossOrderSpec CrossSpaceUcrossOfOrderSpecsUN2(CrossSpace receiver, PtrArray arg1) {
	return receiver.crossOfOrderSpecs(arg1);
/*
udanax-top.st:42469:RequestHandler class methodsFor: 'translate: generated'!
{CrossOrderSpec} CrossSpace.U.crossOfOrderSpecs.U.N2: receiver {CrossSpace} with: arg1 {PtrArray}
	^receiver crossOfOrderSpecs: arg1!
*/
}
public static CrossOrderSpec CrossSpaceUcrossOfOrderSpecsUN3(CrossSpace receiver, PtrArray arg1, PrimIntArray arg2) {
	return receiver.crossOfOrderSpecs(arg1, arg2);
/*
udanax-top.st:42472:RequestHandler class methodsFor: 'translate: generated'!
{CrossOrderSpec} CrossSpace.U.crossOfOrderSpecs.U.N3: receiver {CrossSpace} with: arg1 {PtrArray} with: arg2 {PrimIntArray}
	^receiver crossOfOrderSpecs: arg1 with: arg2!
*/
}
public static Tuple CrossSpaceUcrossOfPositionsUN2(CrossSpace receiver, PtrArray arg1) {
	return receiver.crossOfPositions(arg1);
/*
udanax-top.st:42475:RequestHandler class methodsFor: 'translate: generated'!
{Tuple} CrossSpace.U.crossOfPositions.U.N2: receiver {CrossSpace} with: arg1 {PtrArray}
	^receiver crossOfPositions: arg1!
*/
}
public static CrossRegion CrossSpaceUcrossOfRegionsUN2(CrossSpace receiver, PtrArray arg1) {
	return receiver.crossOfRegions(arg1);
/*
udanax-top.st:42478:RequestHandler class methodsFor: 'translate: generated'!
{CrossRegion} CrossSpace.U.crossOfRegions.U.N2: receiver {CrossSpace} with: arg1 {PtrArray}
	^receiver crossOfRegions: arg1!
*/
}
public static CrossRegion CrossSpaceUextrusionUN3(CrossSpace receiver, PrimIntValue arg1, XnRegion arg2) {
	return receiver.extrusion(arg1.asInt32(), arg2);
/*
udanax-top.st:42481:RequestHandler class methodsFor: 'translate: generated'!
{CrossRegion} CrossSpace.U.extrusion.U.N3: receiver {CrossSpace} with: arg1 {PrimIntValue} with: arg2 {XnRegion}
	^receiver extrusion: arg1 asInt32 with: arg2!
*/
}
public static CrossSpace CrossSpaceUmakeUN1(PtrArray arg1) {
	return CrossSpace.make(arg1);
/*
udanax-top.st:42484:RequestHandler class methodsFor: 'translate: generated'!
{CrossSpace} CrossSpace.U.make.U.N1: arg1 {PtrArray}
	^CrossSpace make: arg1!
*/
}
public static FeDataHolder DataHolderUmakeUN1(PrimValue arg1) {
	return FeDataHolder.make(arg1);
/*
udanax-top.st:42487:RequestHandler class methodsFor: 'translate: generated'!
{FeDataHolder} DataHolder.U.make.U.N1: arg1 {PrimValue}
	^FeDataHolder make: arg1!
*/
}
public static PrimValue DataHolderUvalueUN1(FeDataHolder receiver) {
	return receiver.value();
/*
udanax-top.st:42490:RequestHandler class methodsFor: 'translate: generated'!
{PrimValue} DataHolder.U.value.U.N1: receiver {FeDataHolder}
	^receiver value!
*/
}
public static XnRegion EditionUcanMakeRangeIdenticalUN2(FeEdition receiver, FeEdition arg1) {
	return receiver.canMakeRangeIdentical(arg1);
/*
udanax-top.st:42493:RequestHandler class methodsFor: 'translate: generated'!
{XnRegion} Edition.U.canMakeRangeIdentical.U.N2: receiver {FeEdition} with: arg1 {FeEdition}
	^receiver canMakeRangeIdentical: arg1!
*/
}
public static XnRegion EditionUcanMakeRangeIdenticalUN3(FeEdition receiver, FeEdition arg1, XnRegion arg2) {
	return receiver.canMakeRangeIdentical(arg1, arg2);
/*
udanax-top.st:42496:RequestHandler class methodsFor: 'translate: generated'!
{XnRegion} Edition.U.canMakeRangeIdentical.U.N3: receiver {FeEdition} with: arg1 {FeEdition} with: arg2 {XnRegion}
	^receiver canMakeRangeIdentical: arg1 with: arg2!
*/
}
public static FeEdition EditionUcombineUN2(FeEdition receiver, FeEdition arg1) {
	return receiver.combine(arg1);
/*
udanax-top.st:42499:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} Edition.U.combine.U.N2: receiver {FeEdition} with: arg1 {FeEdition}
	^receiver combine: arg1!
*/
}
public static CoordinateSpace EditionUcoordinateSpaceUN1(FeEdition receiver) {
	return receiver.coordinateSpace();
/*
udanax-top.st:42502:RequestHandler class methodsFor: 'translate: generated'!
{CoordinateSpace} Edition.U.coordinateSpace.U.N1: receiver {FeEdition}
	^receiver coordinateSpace!
*/
}
public static FeEdition EditionUcopyUN2(FeEdition receiver, XnRegion arg1) {
	return receiver.copy(arg1);
/*
udanax-top.st:42505:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} Edition.U.copy.U.N2: receiver {FeEdition} with: arg1 {XnRegion}
	^receiver copy: arg1!
*/
}
public static PrimIntValue EditionUcostUN2(FeEdition receiver, PrimIntValue arg1) {
	return PrimIntValue.make((receiver.cost(arg1.asInt32())));
/*
udanax-top.st:42508:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntValue} Edition.U.cost.U.N2: receiver {FeEdition} with: arg1 {PrimIntValue}
	^PrimIntValue make: (receiver cost: arg1 asInt32)!
*/
}
public static PrimIntValue EditionUcountUN1(FeEdition receiver) {
	return PrimIntValue.make((receiver.count()));
/*
udanax-top.st:42511:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntValue} Edition.U.count.U.N1: receiver {FeEdition}
	^PrimIntValue make: (receiver count)!
*/
}
public static XnRegion EditionUdomainUN1(FeEdition receiver) {
	return receiver.domain();
/*
udanax-top.st:42514:RequestHandler class methodsFor: 'translate: generated'!
{XnRegion} Edition.U.domain.U.N1: receiver {FeEdition}
	^receiver domain!
*/
}
public static FeEdition EditionUemptyUN1(CoordinateSpace arg1) {
	return FeEdition.empty(arg1);
/*
udanax-top.st:42517:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} Edition.U.empty.U.N1: arg1 {CoordinateSpace}
	^FeEdition empty: arg1!
*/
}
public static void EditionUendorseUN2(FeEdition receiver, CrossRegion arg1) {
	receiver.endorse(arg1);
/*
udanax-top.st:42520:RequestHandler class methodsFor: 'translate: generated'!
{void} Edition.U.endorse.U.N2: receiver {FeEdition} with: arg1 {CrossRegion}
	receiver endorse: arg1!
*/
}
public static CrossRegion EditionUendorsementsUN1(FeEdition receiver) {
	return receiver.endorsements();
/*
udanax-top.st:42523:RequestHandler class methodsFor: 'translate: generated'!
{CrossRegion} Edition.U.endorsements.U.N1: receiver {FeEdition}
	^receiver endorsements!
*/
}
public static FeEdition EditionUfromAllUN2(XnRegion arg1, FeRangeElement arg2) {
	return FeEdition.fromAll(arg1, arg2);
/*
udanax-top.st:42526:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} Edition.U.fromAll.U.N2: arg1 {XnRegion} with: arg2 {FeRangeElement}
	^FeEdition fromAll: arg1 with: arg2!
*/
}
public static FeEdition EditionUfromArrayUN1(PrimArray arg1) {
	return FeEdition.fromArray(arg1);
/*
udanax-top.st:42529:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} Edition.U.fromArray.U.N1: arg1 {PrimArray}
	^FeEdition fromArray: arg1!
*/
}
public static FeEdition EditionUfromArrayUN2(PrimArray arg1, XnRegion arg2) {
	return FeEdition.fromArray(arg1, arg2);
/*
udanax-top.st:42532:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} Edition.U.fromArray.U.N2: arg1 {PrimArray} with: arg2 {XnRegion}
	^FeEdition fromArray: arg1 with: arg2!
*/
}
public static FeEdition EditionUfromArrayUN3(PrimArray arg1, XnRegion arg2, OrderSpec arg3) {
	return FeEdition.fromArray(arg1, arg2, arg3);
/*
udanax-top.st:42535:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} Edition.U.fromArray.U.N3: arg1 {PrimArray} with: arg2 {XnRegion} with: arg3 {OrderSpec}
	^FeEdition fromArray: arg1 with: arg2 with: arg3!
*/
}
public static FeEdition EditionUfromOneUN2(Position arg1, FeRangeElement arg2) {
	return FeEdition.fromOne(arg1, arg2);
/*
udanax-top.st:42538:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} Edition.U.fromOne.U.N2: arg1 {Position} with: arg2 {FeRangeElement}
	^FeEdition fromOne: arg1 with: arg2!
*/
}
public static FeRangeElement EditionUgetUN2(FeEdition receiver, Position arg1) {
	return receiver.get(arg1);
/*
udanax-top.st:42541:RequestHandler class methodsFor: 'translate: generated'!
{FeRangeElement} Edition.U.get.U.N2: receiver {FeEdition} with: arg1 {Position}
	^receiver get: arg1!
*/
}
public static boolean EditionUhasPositionUN2(FeEdition receiver, Position arg1) {
	return receiver.hasPosition(arg1);
/*
udanax-top.st:42544:RequestHandler class methodsFor: 'translate: generated'!
{BooleanVar} Edition.U.hasPosition.U.N2: receiver {FeEdition} with: arg1 {Position}
	^receiver hasPosition: arg1!
*/
}
public static boolean EditionUisEmptyUN1(FeEdition receiver) {
	return receiver.isEmpty();
/*
udanax-top.st:42547:RequestHandler class methodsFor: 'translate: generated'!
{BooleanVar} Edition.U.isEmpty.U.N1: receiver {FeEdition}
	^receiver isEmpty!
*/
}
public static boolean EditionUisFiniteUN1(FeEdition receiver) {
	return receiver.isFinite();
/*
udanax-top.st:42550:RequestHandler class methodsFor: 'translate: generated'!
{BooleanVar} Edition.U.isFinite.U.N1: receiver {FeEdition}
	^receiver isFinite!
*/
}
public static boolean EditionUisRangeIdenticalUN2(FeEdition receiver, FeEdition arg1) {
	return receiver.isRangeIdentical(arg1);
/*
udanax-top.st:42553:RequestHandler class methodsFor: 'translate: generated'!
{BooleanVar} Edition.U.isRangeIdentical.U.N2: receiver {FeEdition} with: arg1 {FeEdition}
	^receiver isRangeIdentical: arg1!
*/
}
public static FeEdition EditionUmakeRangeIdenticalUN2(FeEdition receiver, FeEdition arg1) {
	return receiver.makeRangeIdentical(arg1);
/*
udanax-top.st:42556:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} Edition.U.makeRangeIdentical.U.N2: receiver {FeEdition} with: arg1 {FeEdition}
	^receiver makeRangeIdentical: arg1!
*/
}
public static FeEdition EditionUmakeRangeIdenticalUN3(FeEdition receiver, FeEdition arg1, XnRegion arg2) {
	return receiver.makeRangeIdentical(arg1, arg2);
/*
udanax-top.st:42559:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} Edition.U.makeRangeIdentical.U.N3: receiver {FeEdition} with: arg1 {FeEdition} with: arg2 {XnRegion}
	^receiver makeRangeIdentical: arg1 with: arg2!
*/
}
public static Mapping EditionUmapSharedOntoUN2(FeEdition receiver, FeEdition arg1) {
	return receiver.mapSharedOnto(arg1);
/*
udanax-top.st:42562:RequestHandler class methodsFor: 'translate: generated'!
{Mapping} Edition.U.mapSharedOnto.U.N2: receiver {FeEdition} with: arg1 {FeEdition}
	^receiver mapSharedOnto: arg1!
*/
}
public static Mapping EditionUmapSharedToUN2(FeEdition receiver, FeEdition arg1) {
	return receiver.mapSharedTo(arg1);
/*
udanax-top.st:42565:RequestHandler class methodsFor: 'translate: generated'!
{Mapping} Edition.U.mapSharedTo.U.N2: receiver {FeEdition} with: arg1 {FeEdition}
	^receiver mapSharedTo: arg1!
*/
}
public static FeEdition EditionUnotSharedWithUN2(FeEdition receiver, FeEdition arg1) {
	return receiver.notSharedWith(arg1);
/*
udanax-top.st:42568:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} Edition.U.notSharedWith.U.N2: receiver {FeEdition} with: arg1 {FeEdition}
	^receiver notSharedWith: arg1!
*/
}
public static FeEdition EditionUnotSharedWithUN3(FeEdition receiver, FeEdition arg1, PrimIntValue arg2) {
	return receiver.notSharedWith(arg1, arg2.asInt32());
/*
udanax-top.st:42571:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} Edition.U.notSharedWith.U.N3: receiver {FeEdition} with: arg1 {FeEdition} with: arg2 {PrimIntValue}
	^receiver notSharedWith: arg1 with: arg2 asInt32!
*/
}
public static FeEdition EditionUplaceHoldersUN1(XnRegion arg1) {
	return FeEdition.placeHolders(arg1);
/*
udanax-top.st:42574:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} Edition.U.placeHolders.U.N1: arg1 {XnRegion}
	^FeEdition placeHolders: arg1!
*/
}
public static XnRegion EditionUpositionsLabelledUN2(FeEdition receiver, FeLabel arg1) {
	return receiver.positionsLabelled(arg1);
/*
udanax-top.st:42577:RequestHandler class methodsFor: 'translate: generated'!
{XnRegion} Edition.U.positionsLabelled.U.N2: receiver {FeEdition} with: arg1 {FeLabel}
	^receiver positionsLabelled: arg1!
*/
}
public static XnRegion EditionUpositionsOfUN2(FeEdition receiver, FeRangeElement arg1) {
	return receiver.positionsOf(arg1);
/*
udanax-top.st:42580:RequestHandler class methodsFor: 'translate: generated'!
{XnRegion} Edition.U.positionsOf.U.N2: receiver {FeEdition} with: arg1 {FeRangeElement}
	^receiver positionsOf: arg1!
*/
}
public static IDRegion EditionUrangeOwnersUN2(FeEdition receiver, XnRegion arg1) {
	return receiver.rangeOwners(arg1);
/*
udanax-top.st:42583:RequestHandler class methodsFor: 'translate: generated'!
{IDRegion} Edition.U.rangeOwners.U.N2: receiver {FeEdition} with: arg1 {XnRegion}
	^receiver rangeOwners: arg1!
*/
}
public static FeEdition EditionUrangeTranscludersUN1(FeEdition receiver) {
	return receiver.rangeTranscluders();
/*
udanax-top.st:42586:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} Edition.U.rangeTranscluders.U.N1: receiver {FeEdition}
	^receiver rangeTranscluders!
*/
}
public static FeEdition EditionUrangeTranscludersUN2(FeEdition receiver, XnRegion arg1) {
	return receiver.rangeTranscluders(arg1);
/*
udanax-top.st:42589:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} Edition.U.rangeTranscluders.U.N2: receiver {FeEdition} with: arg1 {XnRegion}
	^receiver rangeTranscluders: arg1!
*/
}
public static FeEdition EditionUrangeTranscludersUN3(FeEdition receiver, XnRegion arg1, Filter arg2) {
	return receiver.rangeTranscluders(arg1, arg2);
/*
udanax-top.st:42592:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} Edition.U.rangeTranscluders.U.N3: receiver {FeEdition} with: arg1 {XnRegion} with: arg2 {Filter}
	^receiver rangeTranscluders: arg1 with: arg2!
*/
}
public static FeEdition EditionUrangeTranscludersUN4(FeEdition receiver, XnRegion arg1, Filter arg2, Filter arg3) {
	return receiver.rangeTranscluders(arg1, arg2, arg3);
/*
udanax-top.st:42595:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} Edition.U.rangeTranscluders.U.N4: receiver {FeEdition} with: arg1 {XnRegion} with: arg2 {Filter} with: arg3 {Filter}
	^receiver rangeTranscluders: arg1 with: arg2 with: arg3!
*/
}
public static FeEdition EditionUrangeTranscludersUN5(FeEdition receiver, XnRegion arg1, Filter arg2, Filter arg3, PrimIntValue arg4) {
	return receiver.rangeTranscluders(arg1, arg2, arg3, arg4.asInt32());
/*
udanax-top.st:42598:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} Edition.U.rangeTranscluders.U.N5: receiver {FeEdition} with: arg1 {XnRegion} with: arg2 {Filter} with: arg3 {Filter} with: arg4 {PrimIntValue}
	^receiver rangeTranscluders: arg1 with: arg2 with: arg3 with: arg4 asInt32!
*/
}
public static FeEdition EditionUrangeTranscludersUN6(FeEdition receiver, XnRegion arg1, Filter arg2, Filter arg3, PrimIntValue arg4, FeEdition arg5) {
	return receiver.rangeTranscluders(arg1, arg2, arg3, arg4.asInt32(), arg5);
/*
udanax-top.st:42601:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} Edition.U.rangeTranscluders.U.N6: receiver {FeEdition} with: arg1 {XnRegion} with: arg2 {Filter} with: arg3 {Filter} with: arg4 {PrimIntValue} with: arg5 {FeEdition}
	^receiver rangeTranscluders: arg1 with: arg2 with: arg3 with: arg4 asInt32 with: arg5!
*/
}
public static FeEdition EditionUrangeWorksUN1(FeEdition receiver) {
	return receiver.rangeWorks();
/*
udanax-top.st:42604:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} Edition.U.rangeWorks.U.N1: receiver {FeEdition}
	^receiver rangeWorks!
*/
}
public static FeEdition EditionUrangeWorksUN2(FeEdition receiver, XnRegion arg1) {
	return receiver.rangeWorks(arg1);
/*
udanax-top.st:42607:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} Edition.U.rangeWorks.U.N2: receiver {FeEdition} with: arg1 {XnRegion}
	^receiver rangeWorks: arg1!
*/
}
public static FeEdition EditionUrangeWorksUN3(FeEdition receiver, XnRegion arg1, Filter arg2) {
	return receiver.rangeWorks(arg1, arg2);
/*
udanax-top.st:42610:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} Edition.U.rangeWorks.U.N3: receiver {FeEdition} with: arg1 {XnRegion} with: arg2 {Filter}
	^receiver rangeWorks: arg1 with: arg2!
*/
}
public static FeEdition EditionUrangeWorksUN4(FeEdition receiver, XnRegion arg1, Filter arg2, PrimIntValue arg3) {
	return receiver.rangeWorks(arg1, arg2, arg3.asInt32());
/*
udanax-top.st:42613:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} Edition.U.rangeWorks.U.N4: receiver {FeEdition} with: arg1 {XnRegion} with: arg2 {Filter} with: arg3 {PrimIntValue}
	^receiver rangeWorks: arg1 with: arg2 with: arg3 asInt32!
*/
}
public static FeEdition EditionUrangeWorksUN5(FeEdition receiver, XnRegion arg1, Filter arg2, PrimIntValue arg3, FeEdition arg4) {
	return receiver.rangeWorks(arg1, arg2, arg3.asInt32(), arg4);
/*
udanax-top.st:42616:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} Edition.U.rangeWorks.U.N5: receiver {FeEdition} with: arg1 {XnRegion} with: arg2 {Filter} with: arg3 {PrimIntValue} with: arg4 {FeEdition}
	^receiver rangeWorks: arg1 with: arg2 with: arg3 asInt32 with: arg4!
*/
}
public static FeEdition EditionUrebindUN3(FeEdition receiver, Position arg1, FeEdition arg2) {
	return receiver.rebind(arg1, arg2);
/*
udanax-top.st:42619:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} Edition.U.rebind.U.N3: receiver {FeEdition} with: arg1 {Position} with: arg2 {FeEdition}
	^receiver rebind: arg1 with: arg2!
*/
}
public static FeEdition EditionUreplaceUN2(FeEdition receiver, FeEdition arg1) {
	return receiver.replace(arg1);
/*
udanax-top.st:42622:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} Edition.U.replace.U.N2: receiver {FeEdition} with: arg1 {FeEdition}
	^receiver replace: arg1!
*/
}
public static void EditionUretractUN2(FeEdition receiver, CrossRegion arg1) {
	receiver.retract(arg1);
/*
udanax-top.st:42625:RequestHandler class methodsFor: 'translate: generated'!
{void} Edition.U.retract.U.N2: receiver {FeEdition} with: arg1 {CrossRegion}
	receiver retract: arg1!
*/
}
public static Stepper EditionUretrieveUN1(FeEdition receiver) {
	return receiver.retrieve();
/*
udanax-top.st:42628:RequestHandler class methodsFor: 'translate: generated'!
{Stepper} Edition.U.retrieve.U.N1: receiver {FeEdition}
	^receiver retrieve!
*/
}
public static Stepper EditionUretrieveUN2(FeEdition receiver, XnRegion arg1) {
	return receiver.retrieve(arg1);
/*
udanax-top.st:42631:RequestHandler class methodsFor: 'translate: generated'!
{Stepper} Edition.U.retrieve.U.N2: receiver {FeEdition} with: arg1 {XnRegion}
	^receiver retrieve: arg1!
*/
}
public static Stepper EditionUretrieveUN3(FeEdition receiver, XnRegion arg1, OrderSpec arg2) {
	return receiver.retrieve(arg1, arg2);
/*
udanax-top.st:42634:RequestHandler class methodsFor: 'translate: generated'!
{Stepper} Edition.U.retrieve.U.N3: receiver {FeEdition} with: arg1 {XnRegion} with: arg2 {OrderSpec}
	^receiver retrieve: arg1 with: arg2!
*/
}
public static Stepper EditionUretrieveUN4(FeEdition receiver, XnRegion arg1, OrderSpec arg2, PrimIntValue arg3) {
	return receiver.retrieve(arg1, arg2, arg3.asInt32());
/*
udanax-top.st:42637:RequestHandler class methodsFor: 'translate: generated'!
{Stepper} Edition.U.retrieve.U.N4: receiver {FeEdition} with: arg1 {XnRegion} with: arg2 {OrderSpec} with: arg3 {PrimIntValue}
	^receiver retrieve: arg1 with: arg2 with: arg3 asInt32!
*/
}
public static FeEdition EditionUsetRangeOwnersUN2(FeEdition receiver, ID arg1) {
	return receiver.setRangeOwners(arg1);
/*
udanax-top.st:42640:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} Edition.U.setRangeOwners.U.N2: receiver {FeEdition} with: arg1 {ID}
	^receiver setRangeOwners: arg1!
*/
}
public static FeEdition EditionUsetRangeOwnersUN3(FeEdition receiver, ID arg1, XnRegion arg2) {
	return receiver.setRangeOwners(arg1, arg2);
/*
udanax-top.st:42643:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} Edition.U.setRangeOwners.U.N3: receiver {FeEdition} with: arg1 {ID} with: arg2 {XnRegion}
	^receiver setRangeOwners: arg1 with: arg2!
*/
}
public static XnRegion EditionUsharedRegionUN2(FeEdition receiver, FeEdition arg1) {
	return receiver.sharedRegion(arg1);
/*
udanax-top.st:42646:RequestHandler class methodsFor: 'translate: generated'!
{XnRegion} Edition.U.sharedRegion.U.N2: receiver {FeEdition} with: arg1 {FeEdition}
	^receiver sharedRegion: arg1!
*/
}
public static XnRegion EditionUsharedRegionUN3(FeEdition receiver, FeEdition arg1, PrimIntValue arg2) {
	return receiver.sharedRegion(arg1, arg2.asInt32());
/*
udanax-top.st:42649:RequestHandler class methodsFor: 'translate: generated'!
{XnRegion} Edition.U.sharedRegion.U.N3: receiver {FeEdition} with: arg1 {FeEdition} with: arg2 {PrimIntValue}
	^receiver sharedRegion: arg1 with: arg2 asInt32!
*/
}
public static FeEdition EditionUsharedWithUN2(FeEdition receiver, FeEdition arg1) {
	return receiver.sharedWith(arg1);
/*
udanax-top.st:42652:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} Edition.U.sharedWith.U.N2: receiver {FeEdition} with: arg1 {FeEdition}
	^receiver sharedWith: arg1!
*/
}
public static FeEdition EditionUsharedWithUN3(FeEdition receiver, FeEdition arg1, PrimIntValue arg2) {
	return receiver.sharedWith(arg1, arg2.asInt32());
/*
udanax-top.st:42655:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} Edition.U.sharedWith.U.N3: receiver {FeEdition} with: arg1 {FeEdition} with: arg2 {PrimIntValue}
	^receiver sharedWith: arg1 with: arg2 asInt32!
*/
}
public static TableStepper EditionUstepperUN1(FeEdition receiver) {
	return receiver.stepper();
/*
udanax-top.st:42658:RequestHandler class methodsFor: 'translate: generated'!
{TableStepper} Edition.U.stepper.U.N1: receiver {FeEdition}
	^receiver stepper!
*/
}
public static TableStepper EditionUstepperUN2(FeEdition receiver, XnRegion arg1) {
	return receiver.stepper(arg1);
/*
udanax-top.st:42661:RequestHandler class methodsFor: 'translate: generated'!
{TableStepper} Edition.U.stepper.U.N2: receiver {FeEdition} with: arg1 {XnRegion}
	^receiver stepper: arg1!
*/
}
public static TableStepper EditionUstepperUN3(FeEdition receiver, XnRegion arg1, OrderSpec arg2) {
	return receiver.stepper(arg1, arg2);
/*
udanax-top.st:42664:RequestHandler class methodsFor: 'translate: generated'!
{TableStepper} Edition.U.stepper.U.N3: receiver {FeEdition} with: arg1 {XnRegion} with: arg2 {OrderSpec}
	^receiver stepper: arg1 with: arg2!
*/
}
public static FeRangeElement EditionUtheOneUN1(FeEdition receiver) {
	return receiver.theOne();
/*
udanax-top.st:42667:RequestHandler class methodsFor: 'translate: generated'!
{FeRangeElement} Edition.U.theOne.U.N1: receiver {FeEdition}
	^receiver theOne!
*/
}
public static FeEdition EditionUtransformedByUN2(FeEdition receiver, Mapping arg1) {
	return receiver.transformedBy(arg1);
/*
udanax-top.st:42670:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} Edition.U.transformedBy.U.N2: receiver {FeEdition} with: arg1 {Mapping}
	^receiver transformedBy: arg1!
*/
}
public static CrossRegion EditionUvisibleEndorsementsUN1(FeEdition receiver) {
	return receiver.visibleEndorsements();
/*
udanax-top.st:42673:RequestHandler class methodsFor: 'translate: generated'!
{CrossRegion} Edition.U.visibleEndorsements.U.N1: receiver {FeEdition}
	^receiver visibleEndorsements!
*/
}
public static FeEdition EditionUwithUN3(FeEdition receiver, Position arg1, FeRangeElement arg2) {
	return receiver.with(arg1, arg2);
/*
udanax-top.st:42676:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} Edition.U.with.U.N3: receiver {FeEdition} with: arg1 {Position} with: arg2 {FeRangeElement}
	^receiver with: arg1 with: arg2!
*/
}
public static FeEdition EditionUwithAllUN3(FeEdition receiver, XnRegion arg1, FeRangeElement arg2) {
	return receiver.withAll(arg1, arg2);
/*
udanax-top.st:42679:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} Edition.U.withAll.U.N3: receiver {FeEdition} with: arg1 {XnRegion} with: arg2 {FeRangeElement}
	^receiver withAll: arg1 with: arg2!
*/
}
public static FeEdition EditionUwithoutUN2(FeEdition receiver, Position arg1) {
	return receiver.without(arg1);
/*
udanax-top.st:42682:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} Edition.U.without.U.N2: receiver {FeEdition} with: arg1 {Position}
	^receiver without: arg1!
*/
}
public static FeEdition EditionUwithoutAllUN2(FeEdition receiver, XnRegion arg1) {
	return receiver.withoutAll(arg1);
/*
udanax-top.st:42685:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} Edition.U.withoutAll.U.N2: receiver {FeEdition} with: arg1 {XnRegion}
	^receiver withoutAll: arg1!
*/
}
public static FeRangeElement ElementBundleUelementUN1(FeElementBundle receiver) {
	return receiver.element();
/*
udanax-top.st:42688:RequestHandler class methodsFor: 'translate: generated'!
{FeRangeElement} ElementBundle.U.element.U.N1: receiver {FeElementBundle}
	^receiver element!
*/
}
public static XnRegion FilterUbaseRegionUN1(Filter receiver) {
	return receiver.baseRegion();
/*
udanax-top.st:42691:RequestHandler class methodsFor: 'translate: generated'!
{XnRegion} Filter.U.baseRegion.U.N1: receiver {Filter}
	^receiver baseRegion!
*/
}
public static Stepper FilterUintersectedFiltersUN1(Filter receiver) {
	return receiver.intersectedFilters();
/*
udanax-top.st:42694:RequestHandler class methodsFor: 'translate: generated'!
{Stepper} Filter.U.intersectedFilters.U.N1: receiver {Filter}
	^receiver intersectedFilters!
*/
}
public static boolean FilterUisAllFilterUN1(Filter receiver) {
	return receiver.isAllFilter();
/*
udanax-top.st:42697:RequestHandler class methodsFor: 'translate: generated'!
{BooleanVar} Filter.U.isAllFilter.U.N1: receiver {Filter}
	^receiver isAllFilter!
*/
}
public static boolean FilterUisAnyFilterUN1(Filter receiver) {
	return receiver.isAnyFilter();
/*
udanax-top.st:42700:RequestHandler class methodsFor: 'translate: generated'!
{BooleanVar} Filter.U.isAnyFilter.U.N1: receiver {Filter}
	^receiver isAnyFilter!
*/
}
public static boolean FilterUmatchUN2(Filter receiver, XnRegion arg1) {
	return receiver.match(arg1);
/*
udanax-top.st:42703:RequestHandler class methodsFor: 'translate: generated'!
{BooleanVar} Filter.U.match.U.N2: receiver {Filter} with: arg1 {XnRegion}
	^receiver match: arg1!
*/
}
public static Stepper FilterUunionedFiltersUN1(Filter receiver) {
	return receiver.unionedFilters();
/*
udanax-top.st:42706:RequestHandler class methodsFor: 'translate: generated'!
{Stepper} Filter.U.unionedFilters.U.N1: receiver {Filter}
	^receiver unionedFilters!
*/
}
public static XnRegion FilterPositionUbaseRegionUN1(FilterPosition receiver) {
	return receiver.baseRegion();
/*
udanax-top.st:42709:RequestHandler class methodsFor: 'translate: generated'!
{XnRegion} FilterPosition.U.baseRegion.U.N1: receiver {FilterPosition}
	^receiver baseRegion!
*/
}
public static Filter FilterSpaceUallFilterUN2(FilterSpace receiver, XnRegion arg1) {
	return receiver.allFilter(arg1);
/*
udanax-top.st:42712:RequestHandler class methodsFor: 'translate: generated'!
{Filter} FilterSpace.U.allFilter.U.N2: receiver {FilterSpace} with: arg1 {XnRegion}
	^receiver allFilter: arg1!
*/
}
public static Filter FilterSpaceUanyFilterUN2(FilterSpace receiver, XnRegion arg1) {
	return receiver.anyFilter(arg1);
/*
udanax-top.st:42715:RequestHandler class methodsFor: 'translate: generated'!
{Filter} FilterSpace.U.anyFilter.U.N2: receiver {FilterSpace} with: arg1 {XnRegion}
	^receiver anyFilter: arg1!
*/
}
public static CoordinateSpace FilterSpaceUbaseSpaceUN1(FilterSpace receiver) {
	return receiver.baseSpace();
/*
udanax-top.st:42718:RequestHandler class methodsFor: 'translate: generated'!
{CoordinateSpace} FilterSpace.U.baseSpace.U.N1: receiver {FilterSpace}
	^receiver baseSpace!
*/
}
public static FilterSpace FilterSpaceUmakeUN1(CoordinateSpace arg1) {
	return FilterSpace.make(arg1);
/*
udanax-top.st:42721:RequestHandler class methodsFor: 'translate: generated'!
{FilterSpace} FilterSpace.U.make.U.N1: arg1 {CoordinateSpace}
	^FilterSpace make: arg1!
*/
}
public static FilterPosition FilterSpaceUpositionUN2(FilterSpace receiver, XnRegion arg1) {
	return receiver.position(arg1);
/*
udanax-top.st:42724:RequestHandler class methodsFor: 'translate: generated'!
{FilterPosition} FilterSpace.U.position.U.N2: receiver {FilterSpace} with: arg1 {XnRegion}
	^receiver position: arg1!
*/
}
public static PrimIntValue FloatArrayUbitCountUN1(PrimFloatArray receiver) {
	return PrimIntValue.make((receiver.bitCount()));
/*
udanax-top.st:42727:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntValue} FloatArray.U.bitCount.U.N1: receiver {PrimFloatArray}
	^PrimIntValue make: (receiver bitCount)!
*/
}
public static PrimFloatArray FloatArrayUzerosUN2(PrimIntValue arg1, PrimIntValue arg2) {
	return PrimFloatArray.zeros(arg1.asInt32(), arg2.asInt32());
/*
udanax-top.st:42730:RequestHandler class methodsFor: 'translate: generated'!
{PrimFloatArray} FloatArray.U.zeros.U.N2: arg1 {PrimIntValue} with: arg2 {PrimIntValue}
	^PrimFloatArray zeros: arg1 asInt32 with: arg2 asInt32!
*/
}
public static PrimIntValue FloatValueUbitCountUN1(PrimFloatValue receiver) {
	return PrimIntValue.make((receiver.bitCount()));
/*
udanax-top.st:42733:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntValue} FloatValue.U.bitCount.U.N1: receiver {PrimFloatValue}
	^PrimIntValue make: (receiver bitCount)!
*/
}
public static IntegerVarArray HumberArrayUzerosUN1(PrimIntValue arg1) {
	return IntegerVarArray.zeros(arg1.asInt32());
/*
udanax-top.st:42736:RequestHandler class methodsFor: 'translate: generated'!
{IntegerVarArray} HumberArray.U.zeros.U.N1: arg1 {PrimIntValue}
	^IntegerVarArray zeros: arg1 asInt32!
*/
}
public static FeHyperRef HyperLinkUendAtUN2(FeHyperLink receiver, Sequence arg1) {
	return receiver.endAt(arg1);
/*
udanax-top.st:42739:RequestHandler class methodsFor: 'translate: generated'!
{FeHyperRef} HyperLink.U.endAt.U.N2: receiver {FeHyperLink} with: arg1 {Sequence}
	^receiver endAt: arg1!
*/
}
public static SequenceRegion HyperLinkUendNamesUN1(FeHyperLink receiver) {
	return receiver.endNames();
/*
udanax-top.st:42742:RequestHandler class methodsFor: 'translate: generated'!
{SequenceRegion} HyperLink.U.endNames.U.N1: receiver {FeHyperLink}
	^receiver endNames!
*/
}
public static FeSet HyperLinkUlinkTypesUN1(FeHyperLink receiver) {
	return receiver.linkTypes();
/*
udanax-top.st:42745:RequestHandler class methodsFor: 'translate: generated'!
{FeSet} HyperLink.U.linkTypes.U.N1: receiver {FeHyperLink}
	^receiver linkTypes!
*/
}
public static FeHyperLink HyperLinkUmakeUN3(FeSet arg1, FeHyperRef arg2, FeHyperRef arg3) {
	return FeHyperLink.make(arg1, arg2, arg3);
/*
udanax-top.st:42748:RequestHandler class methodsFor: 'translate: generated'!
{FeHyperLink} HyperLink.U.make.U.N3: arg1 {FeSet} with: arg2 {FeHyperRef} with: arg3 {FeHyperRef}
	^FeHyperLink make: arg1 with: arg2 with: arg3!
*/
}
public static FeHyperLink HyperLinkUwithEndUN3(FeHyperLink receiver, Sequence arg1, FeHyperRef arg2) {
	return receiver.withEnd(arg1, arg2);
/*
udanax-top.st:42751:RequestHandler class methodsFor: 'translate: generated'!
{FeHyperLink} HyperLink.U.withEnd.U.N3: receiver {FeHyperLink} with: arg1 {Sequence} with: arg2 {FeHyperRef}
	^receiver withEnd: arg1 with: arg2!
*/
}
public static FeHyperLink HyperLinkUwithLinkTypesUN2(FeHyperLink receiver, FeSet arg1) {
	return receiver.withLinkTypes(arg1);
/*
udanax-top.st:42754:RequestHandler class methodsFor: 'translate: generated'!
{FeHyperLink} HyperLink.U.withLinkTypes.U.N2: receiver {FeHyperLink} with: arg1 {FeSet}
	^receiver withLinkTypes: arg1!
*/
}
public static FeHyperLink HyperLinkUwithoutEndUN2(FeHyperLink receiver, Sequence arg1) {
	return receiver.withoutEnd(arg1);
/*
udanax-top.st:42757:RequestHandler class methodsFor: 'translate: generated'!
{FeHyperLink} HyperLink.U.withoutEnd.U.N2: receiver {FeHyperLink} with: arg1 {Sequence}
	^receiver withoutEnd: arg1!
*/
}
public static FeWork HyperRefUoriginalContextUN1(FeHyperRef receiver) {
	return receiver.originalContext();
/*
udanax-top.st:42760:RequestHandler class methodsFor: 'translate: generated'!
{FeWork} HyperRef.U.originalContext.U.N1: receiver {FeHyperRef}
	^receiver originalContext!
*/
}
public static FePath HyperRefUpathContextUN1(FeHyperRef receiver) {
	return receiver.pathContext();
/*
udanax-top.st:42763:RequestHandler class methodsFor: 'translate: generated'!
{FePath} HyperRef.U.pathContext.U.N1: receiver {FeHyperRef}
	^receiver pathContext!
*/
}
public static FeHyperRef HyperRefUwithOriginalContextUN2(FeHyperRef receiver, FeWork arg1) {
	return receiver.withOriginalContext(arg1);
/*
udanax-top.st:42766:RequestHandler class methodsFor: 'translate: generated'!
{FeHyperRef} HyperRef.U.withOriginalContext.U.N2: receiver {FeHyperRef} with: arg1 {FeWork}
	^receiver withOriginalContext: arg1!
*/
}
public static FeHyperRef HyperRefUwithPathContextUN2(FeHyperRef receiver, FePath arg1) {
	return receiver.withPathContext(arg1);
/*
udanax-top.st:42769:RequestHandler class methodsFor: 'translate: generated'!
{FeHyperRef} HyperRef.U.withPathContext.U.N2: receiver {FeHyperRef} with: arg1 {FePath}
	^receiver withPathContext: arg1!
*/
}
public static FeHyperRef HyperRefUwithWorkContextUN2(FeHyperRef receiver, FeWork arg1) {
	return receiver.withWorkContext(arg1);
/*
udanax-top.st:42772:RequestHandler class methodsFor: 'translate: generated'!
{FeHyperRef} HyperRef.U.withWorkContext.U.N2: receiver {FeHyperRef} with: arg1 {FeWork}
	^receiver withWorkContext: arg1!
*/
}
public static FeWork HyperRefUworkContextUN1(FeHyperRef receiver) {
	return receiver.workContext();
/*
udanax-top.st:42775:RequestHandler class methodsFor: 'translate: generated'!
{FeWork} HyperRef.U.workContext.U.N1: receiver {FeHyperRef}
	^receiver workContext!
*/
}
public static PrimIntArray IDUexportUN1(ID receiver) {
	return receiver.export();
/*
udanax-top.st:42778:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntArray} ID.U.export.U.N1: receiver {ID}
	^receiver export!
*/
}
public static ID IDUimportUN1(PrimIntArray arg1) {
	return ID.importx(arg1);
/*
udanax-top.st:42781:RequestHandler class methodsFor: 'translate: generated'!
{ID} ID.U.import.U.N1: arg1 {PrimIntArray}
	^ID import: arg1!
*/
}
public static ID IDHolderUiDUN1(FeIDHolder receiver) {
	return receiver.iD();
/*
udanax-top.st:42784:RequestHandler class methodsFor: 'translate: generated'!
{ID} IDHolder.U.iD.U.N1: receiver {FeIDHolder}
	^receiver iD!
*/
}
public static FeIDHolder IDHolderUmakeUN1(ID arg1) {
	return FeIDHolder.make(arg1);
/*
udanax-top.st:42787:RequestHandler class methodsFor: 'translate: generated'!
{FeIDHolder} IDHolder.U.make.U.N1: arg1 {ID}
	^FeIDHolder make: arg1!
*/
}
public static PrimIntArray IDRegionUexportUN1(IDRegion receiver) {
	return receiver.export();
/*
udanax-top.st:42790:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntArray} IDRegion.U.export.U.N1: receiver {IDRegion}
	^receiver export!
*/
}
public static IDRegion IDRegionUimportUN1(PrimIntArray arg1) {
	return IDRegion.importx(arg1);
/*
udanax-top.st:42793:RequestHandler class methodsFor: 'translate: generated'!
{IDRegion} IDRegion.U.import.U.N1: arg1 {PrimIntArray}
	^IDRegion import: arg1!
*/
}
public static PrimIntArray IDSpaceUexportUN1(IDSpace receiver) {
	return receiver.export();
/*
udanax-top.st:42796:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntArray} IDSpace.U.export.U.N1: receiver {IDSpace}
	^receiver export!
*/
}
public static IDSpace IDSpaceUglobalUN0() {
	return IDSpace.global();
/*
udanax-top.st:42799:RequestHandler class methodsFor: 'translate: generated'!
{IDSpace} IDSpace.U.global.U.N0
	^IDSpace global!
*/
}
public static IDRegion IDSpaceUiDsFromServerUN2(IDSpace receiver, Sequence arg1) {
	return receiver.iDsFromServer(arg1);
/*
udanax-top.st:42802:RequestHandler class methodsFor: 'translate: generated'!
{IDRegion} IDSpace.U.iDsFromServer.U.N2: receiver {IDSpace} with: arg1 {Sequence}
	^receiver iDsFromServer: arg1!
*/
}
public static IDSpace IDSpaceUimportUN1(PrimIntArray arg1) {
	return IDSpace.importx(arg1);
/*
udanax-top.st:42805:RequestHandler class methodsFor: 'translate: generated'!
{IDSpace} IDSpace.U.import.U.N1: arg1 {PrimIntArray}
	^IDSpace import: arg1!
*/
}
public static ID IDSpaceUnewIDUN1(IDSpace receiver) {
	return receiver.newID();
/*
udanax-top.st:42808:RequestHandler class methodsFor: 'translate: generated'!
{ID} IDSpace.U.newID.U.N1: receiver {IDSpace}
	^receiver newID!
*/
}
public static IDRegion IDSpaceUnewIDsUN2(IDSpace receiver, PrimIntValue arg1) {
	return receiver.newIDs(arg1.asIntegerVar());
/*
udanax-top.st:42811:RequestHandler class methodsFor: 'translate: generated'!
{IDRegion} IDSpace.U.newIDs.U.N2: receiver {IDSpace} with: arg1 {PrimIntValue}
	^receiver newIDs: arg1 asIntegerVar!
*/
}
public static IDSpace IDSpaceUuniqueUN0() {
	return IDSpace.unique();
/*
udanax-top.st:42814:RequestHandler class methodsFor: 'translate: generated'!
{IDSpace} IDSpace.U.unique.U.N0
	^IDSpace unique!
*/
}
public static PrimIntValue IntArrayUbitCountUN1(PrimIntArray receiver) {
	return PrimIntValue.make((receiver.bitCount()));
/*
udanax-top.st:42817:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntValue} IntArray.U.bitCount.U.N1: receiver {PrimIntArray}
	^PrimIntValue make: (receiver bitCount)!
*/
}
public static PrimIntArray IntArrayUzerosUN2(PrimIntValue arg1, PrimIntValue arg2) {
	return PrimIntArray.zeros(arg1.asInt32(), arg2.asInt32());
/*
udanax-top.st:42820:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntArray} IntArray.U.zeros.U.N2: arg1 {PrimIntValue} with: arg2 {PrimIntValue}
	^PrimIntArray zeros: arg1 asInt32 with: arg2 asInt32!
*/
}
public static PrimIntValue IntegerUvalueUN1(IntegerPos receiver) {
	return PrimIntValue.make((receiver.value()));
/*
udanax-top.st:42823:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntValue} Integer.U.value.U.N1: receiver {IntegerPos}
	^PrimIntValue make: (receiver value)!
*/
}
public static PrimIntValue IntegerMappingUtranslationUN1(IntegerMapping receiver) {
	return PrimIntValue.make((receiver.translation()));
/*
udanax-top.st:42826:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntValue} IntegerMapping.U.translation.U.N1: receiver {IntegerMapping}
	^PrimIntValue make: (receiver translation)!
*/
}
public static Stepper IntegerRegionUintervalsUN1(IntegerRegion receiver) {
	return receiver.intervals();
/*
udanax-top.st:42829:RequestHandler class methodsFor: 'translate: generated'!
{Stepper} IntegerRegion.U.intervals.U.N1: receiver {IntegerRegion}
	^receiver intervals!
*/
}
public static Stepper IntegerRegionUintervalsUN2(IntegerRegion receiver, OrderSpec arg1) {
	return receiver.intervals(arg1);
/*
udanax-top.st:42832:RequestHandler class methodsFor: 'translate: generated'!
{Stepper} IntegerRegion.U.intervals.U.N2: receiver {IntegerRegion} with: arg1 {OrderSpec}
	^receiver intervals: arg1!
*/
}
public static boolean IntegerRegionUisBoundedAboveUN1(IntegerRegion receiver) {
	return receiver.isBoundedAbove();
/*
udanax-top.st:42835:RequestHandler class methodsFor: 'translate: generated'!
{BooleanVar} IntegerRegion.U.isBoundedAbove.U.N1: receiver {IntegerRegion}
	^receiver isBoundedAbove!
*/
}
public static boolean IntegerRegionUisBoundedBelowUN1(IntegerRegion receiver) {
	return receiver.isBoundedBelow();
/*
udanax-top.st:42838:RequestHandler class methodsFor: 'translate: generated'!
{BooleanVar} IntegerRegion.U.isBoundedBelow.U.N1: receiver {IntegerRegion}
	^receiver isBoundedBelow!
*/
}
public static boolean IntegerRegionUisIntervalUN1(IntegerRegion receiver) {
	return receiver.isInterval();
/*
udanax-top.st:42841:RequestHandler class methodsFor: 'translate: generated'!
{BooleanVar} IntegerRegion.U.isInterval.U.N1: receiver {IntegerRegion}
	^receiver isInterval!
*/
}
public static PrimIntValue IntegerRegionUstartUN1(IntegerRegion receiver) {
	return PrimIntValue.make((receiver.start()));
/*
udanax-top.st:42844:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntValue} IntegerRegion.U.start.U.N1: receiver {IntegerRegion}
	^PrimIntValue make: (receiver start)!
*/
}
public static PrimIntValue IntegerRegionUstopUN1(IntegerRegion receiver) {
	return PrimIntValue.make((receiver.stop()));
/*
udanax-top.st:42847:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntValue} IntegerRegion.U.stop.U.N1: receiver {IntegerRegion}
	^PrimIntValue make: (receiver stop)!
*/
}
public static IntegerRegion IntegerSpaceUaboveUN2(IntegerPos arg1, boolean arg2) {
	return IntegerSpace.implicitReceiver().above(arg1, arg2);
/*
udanax-top.st:42850:RequestHandler class methodsFor: 'translate: generated'!
{IntegerRegion} IntegerSpace.U.above.U.N2: arg1 {IntegerPos} with: arg2 {BooleanVar}
	^IntegerSpace implicitReceiver above: arg1 with: arg2!
*/
}
public static IntegerRegion IntegerSpaceUbelowUN2(IntegerPos arg1, boolean arg2) {
	return IntegerSpace.implicitReceiver().below(arg1, arg2);
/*
udanax-top.st:42853:RequestHandler class methodsFor: 'translate: generated'!
{IntegerRegion} IntegerSpace.U.below.U.N2: arg1 {IntegerPos} with: arg2 {BooleanVar}
	^IntegerSpace implicitReceiver below: arg1 with: arg2!
*/
}
public static IntegerRegion IntegerSpaceUintervalUN2(IntegerPos arg1, IntegerPos arg2) {
	return IntegerSpace.implicitReceiver().interval(arg1, arg2);
/*
udanax-top.st:42856:RequestHandler class methodsFor: 'translate: generated'!
{IntegerRegion} IntegerSpace.U.interval.U.N2: arg1 {IntegerPos} with: arg2 {IntegerPos}
	^IntegerSpace implicitReceiver interval: arg1 with: arg2!
*/
}
public static IntegerSpace IntegerSpaceUmakeUN0() {
	return IntegerSpace.make();
/*
udanax-top.st:42859:RequestHandler class methodsFor: 'translate: generated'!
{IntegerSpace} IntegerSpace.U.make.U.N0
	^IntegerSpace make!
*/
}
public static IntegerPos IntegerSpaceUpositionUN1(PrimIntValue arg1) {
	return IntegerSpace.implicitReceiver().position(arg1.asIntegerVar());
/*
udanax-top.st:42862:RequestHandler class methodsFor: 'translate: generated'!
{IntegerPos} IntegerSpace.U.position.U.N1: arg1 {PrimIntValue}
	^IntegerSpace implicitReceiver position: arg1 asIntegerVar!
*/
}
public static IntegerMapping IntegerSpaceUtranslationUN1(PrimIntValue arg1) {
	return IntegerSpace.implicitReceiver().translation(arg1.asIntegerVar());
/*
udanax-top.st:42865:RequestHandler class methodsFor: 'translate: generated'!
{IntegerMapping} IntegerSpace.U.translation.U.N1: arg1 {PrimIntValue}
	^IntegerSpace implicitReceiver translation: arg1 asIntegerVar!
*/
}
public static PrimIntValue IntValueUbitCountUN1(PrimIntValue receiver) {
	return PrimIntValue.make((receiver.bitCount()));
/*
udanax-top.st:42868:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntValue} IntValue.U.bitCount.U.N1: receiver {PrimIntValue}
	^PrimIntValue make: (receiver bitCount)!
*/
}
public static PrimIntValue IntValueUbitwiseAndUN2(PrimIntValue receiver, PrimIntValue arg1) {
	return PrimIntValue.make((receiver.bitwiseAnd(arg1)));
/*
udanax-top.st:42871:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntValue} IntValue.U.bitwiseAnd.U.N2: receiver {PrimIntValue} with: arg1 {PrimIntValue}
	^PrimIntValue make: (receiver bitwiseAnd: arg1)!
*/
}
public static PrimIntValue IntValueUbitwiseOrUN2(PrimIntValue receiver, PrimIntValue arg1) {
	return PrimIntValue.make((receiver.bitwiseOr(arg1)));
/*
udanax-top.st:42874:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntValue} IntValue.U.bitwiseOr.U.N2: receiver {PrimIntValue} with: arg1 {PrimIntValue}
	^PrimIntValue make: (receiver bitwiseOr: arg1)!
*/
}
public static PrimIntValue IntValueUbitwiseXorUN2(PrimIntValue receiver, PrimIntValue arg1) {
	return PrimIntValue.make((receiver.bitwiseXor(arg1)));
/*
udanax-top.st:42877:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntValue} IntValue.U.bitwiseXor.U.N2: receiver {PrimIntValue} with: arg1 {PrimIntValue}
	^PrimIntValue make: (receiver bitwiseXor: arg1)!
*/
}
public static PrimIntValue IntValueUdividedByUN2(PrimIntValue receiver, PrimIntValue arg1) {
	return PrimIntValue.make((receiver.dividedBy(arg1)));
/*
udanax-top.st:42880:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntValue} IntValue.U.dividedBy.U.N2: receiver {PrimIntValue} with: arg1 {PrimIntValue}
	^PrimIntValue make: (receiver dividedBy: arg1)!
*/
}
public static boolean IntValueUisGEUN2(PrimIntValue receiver, PrimIntValue arg1) {
	return receiver.isGE(arg1);
/*
udanax-top.st:42883:RequestHandler class methodsFor: 'translate: generated'!
{BooleanVar} IntValue.U.isGE.U.N2: receiver {PrimIntValue} with: arg1 {PrimIntValue}
	^receiver isGE: arg1!
*/
}
public static PrimIntValue IntValueUleftShiftUN2(PrimIntValue receiver, PrimIntValue arg1) {
	return PrimIntValue.make((receiver.leftShift(arg1)));
/*
udanax-top.st:42886:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntValue} IntValue.U.leftShift.U.N2: receiver {PrimIntValue} with: arg1 {PrimIntValue}
	^PrimIntValue make: (receiver leftShift: arg1)!
*/
}
public static PrimIntValue IntValueUmaximumUN2(PrimIntValue receiver, PrimIntValue arg1) {
	return PrimIntValue.make((receiver.maximum(arg1)));
/*
udanax-top.st:42889:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntValue} IntValue.U.maximum.U.N2: receiver {PrimIntValue} with: arg1 {PrimIntValue}
	^PrimIntValue make: (receiver maximum: arg1)!
*/
}
public static PrimIntValue IntValueUminimumUN2(PrimIntValue receiver, PrimIntValue arg1) {
	return PrimIntValue.make((receiver.minimum(arg1)));
/*
udanax-top.st:42892:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntValue} IntValue.U.minimum.U.N2: receiver {PrimIntValue} with: arg1 {PrimIntValue}
	^PrimIntValue make: (receiver minimum: arg1)!
*/
}
public static PrimIntValue IntValueUminusUN2(PrimIntValue receiver, PrimIntValue arg1) {
	return PrimIntValue.make((receiver.minus(arg1)));
/*
udanax-top.st:42895:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntValue} IntValue.U.minus.U.N2: receiver {PrimIntValue} with: arg1 {PrimIntValue}
	^PrimIntValue make: (receiver minus: arg1)!
*/
}
public static PrimIntValue IntValueUmodUN2(PrimIntValue receiver, PrimIntValue arg1) {
	return PrimIntValue.make((receiver.mod(arg1)));
/*
udanax-top.st:42898:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntValue} IntValue.U.mod.U.N2: receiver {PrimIntValue} with: arg1 {PrimIntValue}
	^PrimIntValue make: (receiver mod: arg1)!
*/
}
public static PrimIntValue IntValueUplusUN2(PrimIntValue receiver, PrimIntValue arg1) {
	return PrimIntValue.make((receiver.plus(arg1)));
/*
udanax-top.st:42901:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntValue} IntValue.U.plus.U.N2: receiver {PrimIntValue} with: arg1 {PrimIntValue}
	^PrimIntValue make: (receiver plus: arg1)!
*/
}
public static PrimIntValue IntValueUtimesUN2(PrimIntValue receiver, PrimIntValue arg1) {
	return PrimIntValue.make((receiver.times(arg1)));
/*
udanax-top.st:42904:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntValue} IntValue.U.times.U.N2: receiver {PrimIntValue} with: arg1 {PrimIntValue}
	^PrimIntValue make: (receiver times: arg1)!
*/
}
public static IDRegion KeyMasterUactualAuthorityUN1(FeKeyMaster receiver) {
	return receiver.actualAuthority();
/*
udanax-top.st:42907:RequestHandler class methodsFor: 'translate: generated'!
{IDRegion} KeyMaster.U.actualAuthority.U.N1: receiver {FeKeyMaster}
	^receiver actualAuthority!
*/
}
public static FeKeyMaster KeyMasterUcopyUN1(FeKeyMaster receiver) {
	return receiver.copy();
/*
udanax-top.st:42910:RequestHandler class methodsFor: 'translate: generated'!
{FeKeyMaster} KeyMaster.U.copy.U.N1: receiver {FeKeyMaster}
	^receiver copy!
*/
}
public static boolean KeyMasterUhasAuthorityUN2(FeKeyMaster receiver, ID arg1) {
	return receiver.hasAuthority(arg1);
/*
udanax-top.st:42913:RequestHandler class methodsFor: 'translate: generated'!
{BooleanVar} KeyMaster.U.hasAuthority.U.N2: receiver {FeKeyMaster} with: arg1 {ID}
	^receiver hasAuthority: arg1!
*/
}
public static void KeyMasterUincorporateUN2(FeKeyMaster receiver, FeKeyMaster arg1) {
	receiver.incorporate(arg1);
/*
udanax-top.st:42916:RequestHandler class methodsFor: 'translate: generated'!
{void} KeyMaster.U.incorporate.U.N2: receiver {FeKeyMaster} with: arg1 {FeKeyMaster}
	receiver incorporate: arg1!
*/
}
public static IDRegion KeyMasterUloginAuthorityUN1(FeKeyMaster receiver) {
	return receiver.loginAuthority();
/*
udanax-top.st:42919:RequestHandler class methodsFor: 'translate: generated'!
{IDRegion} KeyMaster.U.loginAuthority.U.N1: receiver {FeKeyMaster}
	^receiver loginAuthority!
*/
}
public static void KeyMasterUremoveLoginsUN2(FeKeyMaster receiver, IDRegion arg1) {
	receiver.removeLogins(arg1);
/*
udanax-top.st:42922:RequestHandler class methodsFor: 'translate: generated'!
{void} KeyMaster.U.removeLogins.U.N2: receiver {FeKeyMaster} with: arg1 {IDRegion}
	receiver removeLogins: arg1!
*/
}
public static FeLabel LabelUmakeUN0() {
	return FeLabel.make();
/*
udanax-top.st:42925:RequestHandler class methodsFor: 'translate: generated'!
{FeLabel} Label.U.make.U.N0
	^FeLabel make!
*/
}
public static Mapping MappingUcombineUN2(Mapping receiver, Mapping arg1) {
	return receiver.combine(arg1);
/*
udanax-top.st:42928:RequestHandler class methodsFor: 'translate: generated'!
{Mapping} Mapping.U.combine.U.N2: receiver {Mapping} with: arg1 {Mapping}
	^receiver combine: arg1!
*/
}
public static XnRegion MappingUdomainUN1(Mapping receiver) {
	return receiver.domain();
/*
udanax-top.st:42931:RequestHandler class methodsFor: 'translate: generated'!
{XnRegion} Mapping.U.domain.U.N1: receiver {Mapping}
	^receiver domain!
*/
}
public static CoordinateSpace MappingUdomainSpaceUN1(Mapping receiver) {
	return receiver.domainSpace();
/*
udanax-top.st:42934:RequestHandler class methodsFor: 'translate: generated'!
{CoordinateSpace} Mapping.U.domainSpace.U.N1: receiver {Mapping}
	^receiver domainSpace!
*/
}
public static Mapping MappingUinverseUN1(Mapping receiver) {
	return receiver.inverse();
/*
udanax-top.st:42937:RequestHandler class methodsFor: 'translate: generated'!
{Mapping} Mapping.U.inverse.U.N1: receiver {Mapping}
	^receiver inverse!
*/
}
public static boolean MappingUisCompleteUN1(Mapping receiver) {
	return receiver.isComplete();
/*
udanax-top.st:42940:RequestHandler class methodsFor: 'translate: generated'!
{BooleanVar} Mapping.U.isComplete.U.N1: receiver {Mapping}
	^receiver isComplete!
*/
}
public static boolean MappingUisIdentityUN1(Mapping receiver) {
	return receiver.isIdentity();
/*
udanax-top.st:42943:RequestHandler class methodsFor: 'translate: generated'!
{BooleanVar} Mapping.U.isIdentity.U.N1: receiver {Mapping}
	^receiver isIdentity!
*/
}
public static Position MappingUofUN2(Mapping receiver, Position arg1) {
	return receiver.of(arg1);
/*
udanax-top.st:42946:RequestHandler class methodsFor: 'translate: generated'!
{Position} Mapping.U.of.U.N2: receiver {Mapping} with: arg1 {Position}
	^receiver of: arg1!
*/
}
public static XnRegion MappingUofAllUN2(Mapping receiver, XnRegion arg1) {
	return receiver.ofAll(arg1);
/*
udanax-top.st:42949:RequestHandler class methodsFor: 'translate: generated'!
{XnRegion} Mapping.U.ofAll.U.N2: receiver {Mapping} with: arg1 {XnRegion}
	^receiver ofAll: arg1!
*/
}
public static XnRegion MappingUrangeUN1(Mapping receiver) {
	return receiver.range();
/*
udanax-top.st:42952:RequestHandler class methodsFor: 'translate: generated'!
{XnRegion} Mapping.U.range.U.N1: receiver {Mapping}
	^receiver range!
*/
}
public static CoordinateSpace MappingUrangeSpaceUN1(Mapping receiver) {
	return receiver.rangeSpace();
/*
udanax-top.st:42955:RequestHandler class methodsFor: 'translate: generated'!
{CoordinateSpace} Mapping.U.rangeSpace.U.N1: receiver {Mapping}
	^receiver rangeSpace!
*/
}
public static Mapping MappingUrestrictUN2(Mapping receiver, XnRegion arg1) {
	return receiver.restrict(arg1);
/*
udanax-top.st:42958:RequestHandler class methodsFor: 'translate: generated'!
{Mapping} Mapping.U.restrict.U.N2: receiver {Mapping} with: arg1 {XnRegion}
	^receiver restrict: arg1!
*/
}
public static Stepper MappingUsimplerMappingsUN1(Mapping receiver) {
	return receiver.simplerMappings();
/*
udanax-top.st:42961:RequestHandler class methodsFor: 'translate: generated'!
{Stepper} Mapping.U.simplerMappings.U.N1: receiver {Mapping}
	^receiver simplerMappings!
*/
}
public static Mapping MappingUunrestrictedUN1(Mapping receiver) {
	return receiver.unrestricted();
/*
udanax-top.st:42964:RequestHandler class methodsFor: 'translate: generated'!
{Mapping} Mapping.U.unrestricted.U.N1: receiver {Mapping}
	^receiver unrestricted!
*/
}
public static FeKeyMaster MatchLockUencryptedPasswordUN2(MatchLock receiver, PrimIntArray arg1) {
	return receiver.encryptedPassword(arg1);
/*
udanax-top.st:42967:RequestHandler class methodsFor: 'translate: generated'!
{FeKeyMaster} MatchLock.U.encryptedPassword.U.N2: receiver {MatchLock} with: arg1 {PrimIntArray}
	^receiver encryptedPassword: arg1!
*/
}
public static FeMatchLockSmith MatchLockSmithUmakeUN2(PrimIntArray arg1, Sequence arg2) {
	return FeMatchLockSmith.make(arg1, arg2);
/*
udanax-top.st:42970:RequestHandler class methodsFor: 'translate: generated'!
{FeMatchLockSmith} MatchLockSmith.U.make.U.N2: arg1 {PrimIntArray} with: arg2 {Sequence}
	^FeMatchLockSmith make: arg1 with: arg2!
*/
}
public static PrimIntArray MatchLockSmithUscrambledPasswordUN1(FeMatchLockSmith receiver) {
	return receiver.scrambledPassword();
/*
udanax-top.st:42973:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntArray} MatchLockSmith.U.scrambledPassword.U.N1: receiver {FeMatchLockSmith}
	^receiver scrambledPassword!
*/
}
public static PrimIntArray MatchLockSmithUscramblerNameUN1(FeMatchLockSmith receiver) {
	return receiver.scramblerName();
/*
udanax-top.st:42976:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntArray} MatchLockSmith.U.scramblerName.U.N1: receiver {FeMatchLockSmith}
	^receiver scramblerName!
*/
}
public static Lock MultiLockUlockUN2(MultiLock receiver, Sequence arg1) {
	return receiver.lock(arg1);
/*
udanax-top.st:42979:RequestHandler class methodsFor: 'translate: generated'!
{Lock} MultiLock.U.lock.U.N2: receiver {MultiLock} with: arg1 {Sequence}
	^receiver lock: arg1!
*/
}
public static SequenceRegion MultiLockUlockNamesUN1(MultiLock receiver) {
	return receiver.lockNames();
/*
udanax-top.st:42982:RequestHandler class methodsFor: 'translate: generated'!
{SequenceRegion} MultiLock.U.lockNames.U.N1: receiver {MultiLock}
	^receiver lockNames!
*/
}
public static FeLockSmith MultiLockSmithUlockSmithUN2(FeMultiLockSmith receiver, Sequence arg1) {
	return receiver.lockSmith(arg1);
/*
udanax-top.st:42985:RequestHandler class methodsFor: 'translate: generated'!
{FeLockSmith} MultiLockSmith.U.lockSmith.U.N2: receiver {FeMultiLockSmith} with: arg1 {Sequence}
	^receiver lockSmith: arg1!
*/
}
public static SequenceRegion MultiLockSmithUlockSmithNamesUN1(FeMultiLockSmith receiver) {
	return receiver.lockSmithNames();
/*
udanax-top.st:42988:RequestHandler class methodsFor: 'translate: generated'!
{SequenceRegion} MultiLockSmith.U.lockSmithNames.U.N1: receiver {FeMultiLockSmith}
	^receiver lockSmithNames!
*/
}
public static FeMultiLockSmith MultiLockSmithUmakeUN0() {
	return FeMultiLockSmith.make();
/*
udanax-top.st:42991:RequestHandler class methodsFor: 'translate: generated'!
{FeMultiLockSmith} MultiLockSmith.U.make.U.N0
	^FeMultiLockSmith make!
*/
}
public static FeMultiLockSmith MultiLockSmithUwithUN3(FeMultiLockSmith receiver, Sequence arg1, FeLockSmith arg2) {
	return receiver.with(arg1, arg2);
/*
udanax-top.st:42994:RequestHandler class methodsFor: 'translate: generated'!
{FeMultiLockSmith} MultiLockSmith.U.with.U.N3: receiver {FeMultiLockSmith} with: arg1 {Sequence} with: arg2 {FeLockSmith}
	^receiver with: arg1 with: arg2!
*/
}
public static FeMultiLockSmith MultiLockSmithUwithoutUN2(FeMultiLockSmith receiver, Sequence arg1) {
	return receiver.without(arg1);
/*
udanax-top.st:42997:RequestHandler class methodsFor: 'translate: generated'!
{FeMultiLockSmith} MultiLockSmith.U.without.U.N2: receiver {FeMultiLockSmith} with: arg1 {Sequence}
	^receiver without: arg1!
*/
}
public static FeMultiRef MultiRefUintersectUN2(FeMultiRef receiver, FeMultiRef arg1) {
	return receiver.intersect(arg1);
/*
udanax-top.st:43000:RequestHandler class methodsFor: 'translate: generated'!
{FeMultiRef} MultiRef.U.intersect.U.N2: receiver {FeMultiRef} with: arg1 {FeMultiRef}
	^receiver intersect: arg1!
*/
}
public static FeMultiRef MultiRefUmakeUN1(PtrArray arg1) {
	return FeMultiRef.make(arg1);
/*
udanax-top.st:43003:RequestHandler class methodsFor: 'translate: generated'!
{FeMultiRef} MultiRef.U.make.U.N1: arg1 {PtrArray}
	^FeMultiRef make: arg1!
*/
}
public static FeMultiRef MultiRefUmakeUN2(PtrArray arg1, FeWork arg2) {
	return FeMultiRef.make(arg1, arg2);
/*
udanax-top.st:43006:RequestHandler class methodsFor: 'translate: generated'!
{FeMultiRef} MultiRef.U.make.U.N2: arg1 {PtrArray} with: arg2 {FeWork}
	^FeMultiRef make: arg1 with: arg2!
*/
}
public static FeMultiRef MultiRefUmakeUN3(PtrArray arg1, FeWork arg2, FeWork arg3) {
	return FeMultiRef.make(arg1, arg2, arg3);
/*
udanax-top.st:43009:RequestHandler class methodsFor: 'translate: generated'!
{FeMultiRef} MultiRef.U.make.U.N3: arg1 {PtrArray} with: arg2 {FeWork} with: arg3 {FeWork}
	^FeMultiRef make: arg1 with: arg2 with: arg3!
*/
}
public static FeMultiRef MultiRefUmakeUN4(PtrArray arg1, FeWork arg2, FeWork arg3, FePath arg4) {
	return FeMultiRef.make(arg1, arg2, arg3, arg4);
/*
udanax-top.st:43012:RequestHandler class methodsFor: 'translate: generated'!
{FeMultiRef} MultiRef.U.make.U.N4: arg1 {PtrArray} with: arg2 {FeWork} with: arg3 {FeWork} with: arg4 {FePath}
	^FeMultiRef make: arg1 with: arg2 with: arg3 with: arg4!
*/
}
public static FeMultiRef MultiRefUminusUN2(FeMultiRef receiver, FeMultiRef arg1) {
	return receiver.minus(arg1);
/*
udanax-top.st:43015:RequestHandler class methodsFor: 'translate: generated'!
{FeMultiRef} MultiRef.U.minus.U.N2: receiver {FeMultiRef} with: arg1 {FeMultiRef}
	^receiver minus: arg1!
*/
}
public static Stepper MultiRefUrefsUN1(FeMultiRef receiver) {
	return receiver.refs();
/*
udanax-top.st:43018:RequestHandler class methodsFor: 'translate: generated'!
{Stepper} MultiRef.U.refs.U.N1: receiver {FeMultiRef}
	^receiver refs!
*/
}
public static FeMultiRef MultiRefUunionWithUN2(FeMultiRef receiver, FeMultiRef arg1) {
	return receiver.unionWith(arg1);
/*
udanax-top.st:43021:RequestHandler class methodsFor: 'translate: generated'!
{FeMultiRef} MultiRef.U.unionWith.U.N2: receiver {FeMultiRef} with: arg1 {FeMultiRef}
	^receiver unionWith: arg1!
*/
}
public static FeMultiRef MultiRefUwithUN2(FeMultiRef receiver, FeHyperRef arg1) {
	return receiver.with(arg1);
/*
udanax-top.st:43024:RequestHandler class methodsFor: 'translate: generated'!
{FeMultiRef} MultiRef.U.with.U.N2: receiver {FeMultiRef} with: arg1 {FeHyperRef}
	^receiver with: arg1!
*/
}
public static FeMultiRef MultiRefUwithoutUN2(FeMultiRef receiver, FeHyperRef arg1) {
	return receiver.without(arg1);
/*
udanax-top.st:43027:RequestHandler class methodsFor: 'translate: generated'!
{FeMultiRef} MultiRef.U.without.U.N2: receiver {FeMultiRef} with: arg1 {FeHyperRef}
	^receiver without: arg1!
*/
}
public static CoordinateSpace OrderSpecUcoordinateSpaceUN1(OrderSpec receiver) {
	return receiver.coordinateSpace();
/*
udanax-top.st:43030:RequestHandler class methodsFor: 'translate: generated'!
{CoordinateSpace} OrderSpec.U.coordinateSpace.U.N1: receiver {OrderSpec}
	^receiver coordinateSpace!
*/
}
public static boolean OrderSpecUfollowsUN3(OrderSpec receiver, Position arg1, Position arg2) {
	return receiver.follows(arg1, arg2);
/*
udanax-top.st:43033:RequestHandler class methodsFor: 'translate: generated'!
{BooleanVar} OrderSpec.U.follows.U.N3: receiver {OrderSpec} with: arg1 {Position} with: arg2 {Position}
	^receiver follows: arg1 with: arg2!
*/
}
public static OrderSpec OrderSpecUreversedUN1(OrderSpec receiver) {
	return receiver.reversed();
/*
udanax-top.st:43036:RequestHandler class methodsFor: 'translate: generated'!
{OrderSpec} OrderSpec.U.reversed.U.N1: receiver {OrderSpec}
	^receiver reversed!
*/
}
public static FeRangeElement PathUfollowUN2(FePath receiver, FeEdition arg1) {
	return receiver.follow(arg1);
/*
udanax-top.st:43039:RequestHandler class methodsFor: 'translate: generated'!
{FeRangeElement} Path.U.follow.U.N2: receiver {FePath} with: arg1 {FeEdition}
	^receiver follow: arg1!
*/
}
public static FePath PathUmakeUN1(PtrArray arg1) {
	return FePath.make(arg1);
/*
udanax-top.st:43042:RequestHandler class methodsFor: 'translate: generated'!
{FePath} Path.U.make.U.N1: arg1 {PtrArray}
	^FePath make: arg1!
*/
}
public static XnRegion PositionUasRegionUN1(Position receiver) {
	return receiver.asRegion();
/*
udanax-top.st:43045:RequestHandler class methodsFor: 'translate: generated'!
{XnRegion} Position.U.asRegion.U.N1: receiver {Position}
	^receiver asRegion!
*/
}
public static CoordinateSpace PositionUcoordinateSpaceUN1(Position receiver) {
	return receiver.coordinateSpace();
/*
udanax-top.st:43048:RequestHandler class methodsFor: 'translate: generated'!
{CoordinateSpace} Position.U.coordinateSpace.U.N1: receiver {Position}
	^receiver coordinateSpace!
*/
}
public static PtrArray PtrArrayUnullsUN1(PrimIntValue arg1) {
	return PtrArray.nulls(arg1.asInt32());
/*
udanax-top.st:43051:RequestHandler class methodsFor: 'translate: generated'!
{PtrArray} PtrArray.U.nulls.U.N1: arg1 {PrimIntValue}
	^PtrArray nulls: arg1 asInt32!
*/
}
public static FeRangeElement RangeElementUagainUN1(FeRangeElement receiver) {
	return receiver.again();
/*
udanax-top.st:43054:RequestHandler class methodsFor: 'translate: generated'!
{FeRangeElement} RangeElement.U.again.U.N1: receiver {FeRangeElement}
	^receiver again!
*/
}
public static boolean RangeElementUcanMakeIdenticalUN2(FeRangeElement receiver, FeRangeElement arg1) {
	return receiver.canMakeIdentical(arg1);
/*
udanax-top.st:43057:RequestHandler class methodsFor: 'translate: generated'!
{BooleanVar} RangeElement.U.canMakeIdentical.U.N2: receiver {FeRangeElement} with: arg1 {FeRangeElement}
	^receiver canMakeIdentical: arg1!
*/
}
public static boolean RangeElementUisIdenticalUN2(FeRangeElement receiver, FeRangeElement arg1) {
	return receiver.isIdentical(arg1);
/*
udanax-top.st:43060:RequestHandler class methodsFor: 'translate: generated'!
{BooleanVar} RangeElement.U.isIdentical.U.N2: receiver {FeRangeElement} with: arg1 {FeRangeElement}
	^receiver isIdentical: arg1!
*/
}
public static FeLabel RangeElementUlabelUN1(FeRangeElement receiver) {
	return receiver.label();
/*
udanax-top.st:43063:RequestHandler class methodsFor: 'translate: generated'!
{FeLabel} RangeElement.U.label.U.N1: receiver {FeRangeElement}
	^receiver label!
*/
}
public static void RangeElementUmakeIdenticalUN2(FeRangeElement receiver, FeRangeElement arg1) {
	receiver.makeIdentical(arg1);
/*
udanax-top.st:43066:RequestHandler class methodsFor: 'translate: generated'!
{void} RangeElement.U.makeIdentical.U.N2: receiver {FeRangeElement} with: arg1 {FeRangeElement}
	receiver makeIdentical: arg1!
*/
}
public static ID RangeElementUownerUN1(FeRangeElement receiver) {
	return receiver.owner();
/*
udanax-top.st:43069:RequestHandler class methodsFor: 'translate: generated'!
{ID} RangeElement.U.owner.U.N1: receiver {FeRangeElement}
	^receiver owner!
*/
}
public static FeRangeElement RangeElementUplaceHolderUN0() {
	return FeRangeElement.placeHolder();
/*
udanax-top.st:43072:RequestHandler class methodsFor: 'translate: generated'!
{FeRangeElement} RangeElement.U.placeHolder.U.N0
	^FeRangeElement placeHolder!
*/
}
public static FeRangeElement RangeElementUrelabelledUN2(FeRangeElement receiver, FeLabel arg1) {
	return receiver.relabelled(arg1);
/*
udanax-top.st:43075:RequestHandler class methodsFor: 'translate: generated'!
{FeRangeElement} RangeElement.U.relabelled.U.N2: receiver {FeRangeElement} with: arg1 {FeLabel}
	^receiver relabelled: arg1!
*/
}
public static void RangeElementUsetOwnerUN2(FeRangeElement receiver, ID arg1) {
	receiver.setOwner(arg1);
/*
udanax-top.st:43078:RequestHandler class methodsFor: 'translate: generated'!
{void} RangeElement.U.setOwner.U.N2: receiver {FeRangeElement} with: arg1 {ID}
	receiver setOwner: arg1!
*/
}
public static FeEdition RangeElementUtranscludersUN1(FeRangeElement receiver) {
	return receiver.transcluders();
/*
udanax-top.st:43081:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} RangeElement.U.transcluders.U.N1: receiver {FeRangeElement}
	^receiver transcluders!
*/
}
public static FeEdition RangeElementUtranscludersUN2(FeRangeElement receiver, Filter arg1) {
	return receiver.transcluders(arg1);
/*
udanax-top.st:43084:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} RangeElement.U.transcluders.U.N2: receiver {FeRangeElement} with: arg1 {Filter}
	^receiver transcluders: arg1!
*/
}
public static FeEdition RangeElementUtranscludersUN3(FeRangeElement receiver, Filter arg1, Filter arg2) {
	return receiver.transcluders(arg1, arg2);
/*
udanax-top.st:43087:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} RangeElement.U.transcluders.U.N3: receiver {FeRangeElement} with: arg1 {Filter} with: arg2 {Filter}
	^receiver transcluders: arg1 with: arg2!
*/
}
public static FeEdition RangeElementUtranscludersUN4(FeRangeElement receiver, Filter arg1, Filter arg2, PrimIntValue arg3) {
	return receiver.transcluders(arg1, arg2, arg3.asInt32());
/*
udanax-top.st:43090:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} RangeElement.U.transcluders.U.N4: receiver {FeRangeElement} with: arg1 {Filter} with: arg2 {Filter} with: arg3 {PrimIntValue}
	^receiver transcluders: arg1 with: arg2 with: arg3 asInt32!
*/
}
public static FeEdition RangeElementUtranscludersUN5(FeRangeElement receiver, Filter arg1, Filter arg2, PrimIntValue arg3, FeEdition arg4) {
	return receiver.transcluders(arg1, arg2, arg3.asInt32(), arg4);
/*
udanax-top.st:43093:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} RangeElement.U.transcluders.U.N5: receiver {FeRangeElement} with: arg1 {Filter} with: arg2 {Filter} with: arg3 {PrimIntValue} with: arg4 {FeEdition}
	^receiver transcluders: arg1 with: arg2 with: arg3 asInt32 with: arg4!
*/
}
public static FeEdition RangeElementUworksUN1(FeRangeElement receiver) {
	return receiver.works();
/*
udanax-top.st:43096:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} RangeElement.U.works.U.N1: receiver {FeRangeElement}
	^receiver works!
*/
}
public static FeEdition RangeElementUworksUN2(FeRangeElement receiver, Filter arg1) {
	return receiver.works(arg1);
/*
udanax-top.st:43099:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} RangeElement.U.works.U.N2: receiver {FeRangeElement} with: arg1 {Filter}
	^receiver works: arg1!
*/
}
public static FeEdition RangeElementUworksUN3(FeRangeElement receiver, Filter arg1, PrimIntValue arg2) {
	return receiver.works(arg1, arg2.asInt32());
/*
udanax-top.st:43102:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} RangeElement.U.works.U.N3: receiver {FeRangeElement} with: arg1 {Filter} with: arg2 {PrimIntValue}
	^receiver works: arg1 with: arg2 asInt32!
*/
}
public static FeEdition RangeElementUworksUN4(FeRangeElement receiver, Filter arg1, PrimIntValue arg2, FeEdition arg3) {
	return receiver.works(arg1, arg2.asInt32(), arg3);
/*
udanax-top.st:43105:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} RangeElement.U.works.U.N4: receiver {FeRangeElement} with: arg1 {Filter} with: arg2 {PrimIntValue} with: arg3 {FeEdition}
	^receiver works: arg1 with: arg2 asInt32 with: arg3!
*/
}
public static PrimFloatValue RealUvalueUN1(RealPos receiver) {
	return receiver.value();
/*
udanax-top.st:43108:RequestHandler class methodsFor: 'translate: generated'!
{PrimFloatValue} Real.U.value.U.N1: receiver {RealPos}
	^receiver value!
*/
}
public static Stepper RealRegionUintervalsUN1(RealRegion receiver) {
	return receiver.intervals();
/*
udanax-top.st:43111:RequestHandler class methodsFor: 'translate: generated'!
{Stepper} RealRegion.U.intervals.U.N1: receiver {RealRegion}
	^receiver intervals!
*/
}
public static Stepper RealRegionUintervalsUN2(RealRegion receiver, OrderSpec arg1) {
	return receiver.intervals(arg1);
/*
udanax-top.st:43114:RequestHandler class methodsFor: 'translate: generated'!
{Stepper} RealRegion.U.intervals.U.N2: receiver {RealRegion} with: arg1 {OrderSpec}
	^receiver intervals: arg1!
*/
}
public static boolean RealRegionUisBoundedAboveUN1(RealRegion receiver) {
	return receiver.isBoundedAbove();
/*
udanax-top.st:43117:RequestHandler class methodsFor: 'translate: generated'!
{BooleanVar} RealRegion.U.isBoundedAbove.U.N1: receiver {RealRegion}
	^receiver isBoundedAbove!
*/
}
public static boolean RealRegionUisBoundedBelowUN1(RealRegion receiver) {
	return receiver.isBoundedBelow();
/*
udanax-top.st:43120:RequestHandler class methodsFor: 'translate: generated'!
{BooleanVar} RealRegion.U.isBoundedBelow.U.N1: receiver {RealRegion}
	^receiver isBoundedBelow!
*/
}
public static boolean RealRegionUisIntervalUN1(RealRegion receiver) {
	return receiver.isInterval();
/*
udanax-top.st:43123:RequestHandler class methodsFor: 'translate: generated'!
{BooleanVar} RealRegion.U.isInterval.U.N1: receiver {RealRegion}
	^receiver isInterval!
*/
}
public static RealPos RealRegionUlowerBoundUN1(RealRegion receiver) {
	return receiver.lowerBound();
/*
udanax-top.st:43126:RequestHandler class methodsFor: 'translate: generated'!
{RealPos} RealRegion.U.lowerBound.U.N1: receiver {RealRegion}
	^receiver lowerBound!
*/
}
public static RealPos RealRegionUupperBoundUN1(RealRegion receiver) {
	return receiver.upperBound();
/*
udanax-top.st:43129:RequestHandler class methodsFor: 'translate: generated'!
{RealPos} RealRegion.U.upperBound.U.N1: receiver {RealRegion}
	^receiver upperBound!
*/
}
public static RealRegion RealSpaceUaboveUN3(RealSpace receiver, RealPos arg1, boolean arg2) {
	return receiver.above(arg1, arg2);
/*
udanax-top.st:43132:RequestHandler class methodsFor: 'translate: generated'!
{RealRegion} RealSpace.U.above.U.N3: receiver {RealSpace} with: arg1 {RealPos} with: arg2 {BooleanVar}
	^receiver above: arg1 with: arg2!
*/
}
public static RealRegion RealSpaceUbelowUN3(RealSpace receiver, RealPos arg1, boolean arg2) {
	return receiver.below(arg1, arg2);
/*
udanax-top.st:43135:RequestHandler class methodsFor: 'translate: generated'!
{RealRegion} RealSpace.U.below.U.N3: receiver {RealSpace} with: arg1 {RealPos} with: arg2 {BooleanVar}
	^receiver below: arg1 with: arg2!
*/
}
public static RealRegion RealSpaceUintervalUN3(RealSpace receiver, RealPos arg1, RealPos arg2) {
	return receiver.interval(arg1, arg2);
/*
udanax-top.st:43138:RequestHandler class methodsFor: 'translate: generated'!
{RealRegion} RealSpace.U.interval.U.N3: receiver {RealSpace} with: arg1 {RealPos} with: arg2 {RealPos}
	^receiver interval: arg1 with: arg2!
*/
}
public static RealSpace RealSpaceUmakeUN0() {
	return RealSpace.make();
/*
udanax-top.st:43141:RequestHandler class methodsFor: 'translate: generated'!
{RealSpace} RealSpace.U.make.U.N0
	^RealSpace make!
*/
}
public static RealPos RealSpaceUpositionUN2(RealSpace receiver, PrimFloatValue arg1) {
	return receiver.position(arg1.asIEEE64());
/*
udanax-top.st:43144:RequestHandler class methodsFor: 'translate: generated'!
{RealPos} RealSpace.U.position.U.N2: receiver {RealSpace} with: arg1 {PrimFloatValue}
	^receiver position: arg1 asIEEE64!
*/
}
public static XnRegion RegionUchooseManyUN2(XnRegion receiver, PrimIntValue arg1) {
	return receiver.chooseMany(arg1.asIntegerVar());
/*
udanax-top.st:43147:RequestHandler class methodsFor: 'translate: generated'!
{XnRegion} Region.U.chooseMany.U.N2: receiver {XnRegion} with: arg1 {PrimIntValue}
	^receiver chooseMany: arg1 asIntegerVar!
*/
}
public static XnRegion RegionUchooseManyUN3(XnRegion receiver, PrimIntValue arg1, OrderSpec arg2) {
	return receiver.chooseMany(arg1.asIntegerVar(), arg2);
/*
udanax-top.st:43150:RequestHandler class methodsFor: 'translate: generated'!
{XnRegion} Region.U.chooseMany.U.N3: receiver {XnRegion} with: arg1 {PrimIntValue} with: arg2 {OrderSpec}
	^receiver chooseMany: arg1 asIntegerVar with: arg2!
*/
}
public static Position RegionUchooseOneUN1(XnRegion receiver) {
	return receiver.chooseOne();
/*
udanax-top.st:43153:RequestHandler class methodsFor: 'translate: generated'!
{Position} Region.U.chooseOne.U.N1: receiver {XnRegion}
	^receiver chooseOne!
*/
}
public static Position RegionUchooseOneUN2(XnRegion receiver, OrderSpec arg1) {
	return receiver.chooseOne(arg1);
/*
udanax-top.st:43156:RequestHandler class methodsFor: 'translate: generated'!
{Position} Region.U.chooseOne.U.N2: receiver {XnRegion} with: arg1 {OrderSpec}
	^receiver chooseOne: arg1!
*/
}
public static XnRegion RegionUcomplementUN1(XnRegion receiver) {
	return receiver.complement();
/*
udanax-top.st:43159:RequestHandler class methodsFor: 'translate: generated'!
{XnRegion} Region.U.complement.U.N1: receiver {XnRegion}
	^receiver complement!
*/
}
public static CoordinateSpace RegionUcoordinateSpaceUN1(XnRegion receiver) {
	return receiver.coordinateSpace();
/*
udanax-top.st:43162:RequestHandler class methodsFor: 'translate: generated'!
{CoordinateSpace} Region.U.coordinateSpace.U.N1: receiver {XnRegion}
	^receiver coordinateSpace!
*/
}
public static PrimIntValue RegionUcountUN1(XnRegion receiver) {
	return PrimIntValue.make((receiver.count()));
/*
udanax-top.st:43165:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntValue} Region.U.count.U.N1: receiver {XnRegion}
	^PrimIntValue make: (receiver count)!
*/
}
public static boolean RegionUhasMemberUN2(XnRegion receiver, Position arg1) {
	return receiver.hasMember(arg1);
/*
udanax-top.st:43168:RequestHandler class methodsFor: 'translate: generated'!
{BooleanVar} Region.U.hasMember.U.N2: receiver {XnRegion} with: arg1 {Position}
	^receiver hasMember: arg1!
*/
}
public static XnRegion RegionUintersectUN2(XnRegion receiver, XnRegion arg1) {
	return receiver.intersect(arg1);
/*
udanax-top.st:43171:RequestHandler class methodsFor: 'translate: generated'!
{XnRegion} Region.U.intersect.U.N2: receiver {XnRegion} with: arg1 {XnRegion}
	^receiver intersect: arg1!
*/
}
public static boolean RegionUintersectsUN2(XnRegion receiver, XnRegion arg1) {
	return receiver.intersects(arg1);
/*
udanax-top.st:43174:RequestHandler class methodsFor: 'translate: generated'!
{BooleanVar} Region.U.intersects.U.N2: receiver {XnRegion} with: arg1 {XnRegion}
	^receiver intersects: arg1!
*/
}
public static boolean RegionUisEmptyUN1(XnRegion receiver) {
	return receiver.isEmpty();
/*
udanax-top.st:43177:RequestHandler class methodsFor: 'translate: generated'!
{BooleanVar} Region.U.isEmpty.U.N1: receiver {XnRegion}
	^receiver isEmpty!
*/
}
public static boolean RegionUisFiniteUN1(XnRegion receiver) {
	return receiver.isFinite();
/*
udanax-top.st:43180:RequestHandler class methodsFor: 'translate: generated'!
{BooleanVar} Region.U.isFinite.U.N1: receiver {XnRegion}
	^receiver isFinite!
*/
}
public static boolean RegionUisFullUN1(XnRegion receiver) {
	return receiver.isFull();
/*
udanax-top.st:43183:RequestHandler class methodsFor: 'translate: generated'!
{BooleanVar} Region.U.isFull.U.N1: receiver {XnRegion}
	^receiver isFull!
*/
}
public static boolean RegionUisSubsetOfUN2(XnRegion receiver, XnRegion arg1) {
	return receiver.isSubsetOf(arg1);
/*
udanax-top.st:43186:RequestHandler class methodsFor: 'translate: generated'!
{BooleanVar} Region.U.isSubsetOf.U.N2: receiver {XnRegion} with: arg1 {XnRegion}
	^receiver isSubsetOf: arg1!
*/
}
public static XnRegion RegionUminusUN2(XnRegion receiver, XnRegion arg1) {
	return receiver.minus(arg1);
/*
udanax-top.st:43189:RequestHandler class methodsFor: 'translate: generated'!
{XnRegion} Region.U.minus.U.N2: receiver {XnRegion} with: arg1 {XnRegion}
	^receiver minus: arg1!
*/
}
public static Stepper RegionUstepperUN1(XnRegion receiver) {
	return receiver.stepper();
/*
udanax-top.st:43192:RequestHandler class methodsFor: 'translate: generated'!
{Stepper} Region.U.stepper.U.N1: receiver {XnRegion}
	^receiver stepper!
*/
}
public static Stepper RegionUstepperUN2(XnRegion receiver, OrderSpec arg1) {
	return receiver.stepper(arg1);
/*
udanax-top.st:43195:RequestHandler class methodsFor: 'translate: generated'!
{Stepper} Region.U.stepper.U.N2: receiver {XnRegion} with: arg1 {OrderSpec}
	^receiver stepper: arg1!
*/
}
public static Position RegionUtheOneUN1(XnRegion receiver) {
	return receiver.theOne();
/*
udanax-top.st:43198:RequestHandler class methodsFor: 'translate: generated'!
{Position} Region.U.theOne.U.N1: receiver {XnRegion}
	^receiver theOne!
*/
}
public static XnRegion RegionUunionWithUN2(XnRegion receiver, XnRegion arg1) {
	return receiver.unionWith(arg1);
/*
udanax-top.st:43201:RequestHandler class methodsFor: 'translate: generated'!
{XnRegion} Region.U.unionWith.U.N2: receiver {XnRegion} with: arg1 {XnRegion}
	^receiver unionWith: arg1!
*/
}
public static XnRegion RegionUwithUN2(XnRegion receiver, Position arg1) {
	return receiver.with(arg1);
/*
udanax-top.st:43204:RequestHandler class methodsFor: 'translate: generated'!
{XnRegion} Region.U.with.U.N2: receiver {XnRegion} with: arg1 {Position}
	^receiver with: arg1!
*/
}
public static XnRegion RegionUwithoutUN2(XnRegion receiver, Position arg1) {
	return receiver.without(arg1);
/*
udanax-top.st:43207:RequestHandler class methodsFor: 'translate: generated'!
{XnRegion} Region.U.without.U.N2: receiver {XnRegion} with: arg1 {Position}
	^receiver without: arg1!
*/
}
public static PrimIntValue SequenceUfirstIndexUN1(Sequence receiver) {
	return PrimIntValue.make((receiver.firstIndex()));
/*
udanax-top.st:43210:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntValue} Sequence.U.firstIndex.U.N1: receiver {Sequence}
	^PrimIntValue make: (receiver firstIndex)!
*/
}
public static PrimIntValue SequenceUintegerAtUN2(Sequence receiver, PrimIntValue arg1) {
	return PrimIntValue.make((receiver.integerAt(arg1.asIntegerVar())));
/*
udanax-top.st:43213:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntValue} Sequence.U.integerAt.U.N2: receiver {Sequence} with: arg1 {PrimIntValue}
	^PrimIntValue make: (receiver integerAt: arg1 asIntegerVar)!
*/
}
public static PrimArray SequenceUintegersUN1(Sequence receiver) {
	return receiver.integers();
/*
udanax-top.st:43216:RequestHandler class methodsFor: 'translate: generated'!
{PrimArray} Sequence.U.integers.U.N1: receiver {Sequence}
	^receiver integers!
*/
}
public static boolean SequenceUisZeroUN1(Sequence receiver) {
	return receiver.isZero();
/*
udanax-top.st:43219:RequestHandler class methodsFor: 'translate: generated'!
{BooleanVar} Sequence.U.isZero.U.N1: receiver {Sequence}
	^receiver isZero!
*/
}
public static PrimIntValue SequenceUlastIndexUN1(Sequence receiver) {
	return PrimIntValue.make((receiver.lastIndex()));
/*
udanax-top.st:43222:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntValue} Sequence.U.lastIndex.U.N1: receiver {Sequence}
	^PrimIntValue make: (receiver lastIndex)!
*/
}
public static Sequence SequenceUwithUN3(Sequence receiver, PrimIntValue arg1, PrimIntValue arg2) {
	return receiver.with(arg1.asIntegerVar(), arg2.asIntegerVar());
/*
udanax-top.st:43225:RequestHandler class methodsFor: 'translate: generated'!
{Sequence} Sequence.U.with.U.N3: receiver {Sequence} with: arg1 {PrimIntValue} with: arg2 {PrimIntValue}
	^receiver with: arg1 asIntegerVar with: arg2 asIntegerVar!
*/
}
public static PrimIntValue SequenceMappingUshiftUN1(SequenceMapping receiver) {
	return PrimIntValue.make((receiver.shift()));
/*
udanax-top.st:43228:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntValue} SequenceMapping.U.shift.U.N1: receiver {SequenceMapping}
	^PrimIntValue make: (receiver shift)!
*/
}
public static Sequence SequenceMappingUtranslationUN1(SequenceMapping receiver) {
	return receiver.translation();
/*
udanax-top.st:43231:RequestHandler class methodsFor: 'translate: generated'!
{Sequence} SequenceMapping.U.translation.U.N1: receiver {SequenceMapping}
	^receiver translation!
*/
}
public static Stepper SequenceRegionUintervalsUN1(SequenceRegion receiver) {
	return receiver.intervals();
/*
udanax-top.st:43234:RequestHandler class methodsFor: 'translate: generated'!
{Stepper} SequenceRegion.U.intervals.U.N1: receiver {SequenceRegion}
	^receiver intervals!
*/
}
public static Stepper SequenceRegionUintervalsUN2(SequenceRegion receiver, OrderSpec arg1) {
	return receiver.intervals(arg1);
/*
udanax-top.st:43237:RequestHandler class methodsFor: 'translate: generated'!
{Stepper} SequenceRegion.U.intervals.U.N2: receiver {SequenceRegion} with: arg1 {OrderSpec}
	^receiver intervals: arg1!
*/
}
public static boolean SequenceRegionUisBoundedAboveUN1(SequenceRegion receiver) {
	return receiver.isBoundedAbove();
/*
udanax-top.st:43240:RequestHandler class methodsFor: 'translate: generated'!
{BooleanVar} SequenceRegion.U.isBoundedAbove.U.N1: receiver {SequenceRegion}
	^receiver isBoundedAbove!
*/
}
public static boolean SequenceRegionUisBoundedBelowUN1(SequenceRegion receiver) {
	return receiver.isBoundedBelow();
/*
udanax-top.st:43243:RequestHandler class methodsFor: 'translate: generated'!
{BooleanVar} SequenceRegion.U.isBoundedBelow.U.N1: receiver {SequenceRegion}
	^receiver isBoundedBelow!
*/
}
public static boolean SequenceRegionUisIntervalUN1(SequenceRegion receiver) {
	return receiver.isInterval();
/*
udanax-top.st:43246:RequestHandler class methodsFor: 'translate: generated'!
{BooleanVar} SequenceRegion.U.isInterval.U.N1: receiver {SequenceRegion}
	^receiver isInterval!
*/
}
public static Sequence SequenceRegionUlowerEdgeUN1(SequenceRegion receiver) {
	return receiver.lowerEdge();
/*
udanax-top.st:43249:RequestHandler class methodsFor: 'translate: generated'!
{Sequence} SequenceRegion.U.lowerEdge.U.N1: receiver {SequenceRegion}
	^receiver lowerEdge!
*/
}
public static PrimIntValue SequenceRegionUlowerEdgePrefixLimitUN1(SequenceRegion receiver) {
	return PrimIntValue.make((receiver.lowerEdgePrefixLimit()));
/*
udanax-top.st:43252:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntValue} SequenceRegion.U.lowerEdgePrefixLimit.U.N1: receiver {SequenceRegion}
	^PrimIntValue make: (receiver lowerEdgePrefixLimit)!
*/
}
public static PrimIntValue SequenceRegionUlowerEdgeTypeUN1(SequenceRegion receiver) {
	return PrimIntValue.make((receiver.lowerEdgeType()));
/*
udanax-top.st:43255:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntValue} SequenceRegion.U.lowerEdgeType.U.N1: receiver {SequenceRegion}
	^PrimIntValue make: (receiver lowerEdgeType)!
*/
}
public static Sequence SequenceRegionUupperEdgeUN1(SequenceRegion receiver) {
	return receiver.upperEdge();
/*
udanax-top.st:43258:RequestHandler class methodsFor: 'translate: generated'!
{Sequence} SequenceRegion.U.upperEdge.U.N1: receiver {SequenceRegion}
	^receiver upperEdge!
*/
}
public static PrimIntValue SequenceRegionUupperEdgePrefixLimitUN1(SequenceRegion receiver) {
	return PrimIntValue.make((receiver.upperEdgePrefixLimit()));
/*
udanax-top.st:43261:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntValue} SequenceRegion.U.upperEdgePrefixLimit.U.N1: receiver {SequenceRegion}
	^PrimIntValue make: (receiver upperEdgePrefixLimit)!
*/
}
public static PrimIntValue SequenceRegionUupperEdgeTypeUN1(SequenceRegion receiver) {
	return PrimIntValue.make((receiver.upperEdgeType()));
/*
udanax-top.st:43264:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntValue} SequenceRegion.U.upperEdgeType.U.N1: receiver {SequenceRegion}
	^PrimIntValue make: (receiver upperEdgeType)!
*/
}
public static SequenceRegion SequenceSpaceUaboveUN2(Sequence arg1, boolean arg2) {
	return SequenceSpace.implicitReceiver().above(arg1, arg2);
/*
udanax-top.st:43267:RequestHandler class methodsFor: 'translate: generated'!
{SequenceRegion} SequenceSpace.U.above.U.N2: arg1 {Sequence} with: arg2 {BooleanVar}
	^SequenceSpace implicitReceiver above: arg1 with: arg2!
*/
}
public static SequenceRegion SequenceSpaceUbelowUN2(Sequence arg1, boolean arg2) {
	return SequenceSpace.implicitReceiver().below(arg1, arg2);
/*
udanax-top.st:43270:RequestHandler class methodsFor: 'translate: generated'!
{SequenceRegion} SequenceSpace.U.below.U.N2: arg1 {Sequence} with: arg2 {BooleanVar}
	^SequenceSpace implicitReceiver below: arg1 with: arg2!
*/
}
public static SequenceRegion SequenceSpaceUintervalUN2(Sequence arg1, Sequence arg2) {
	return SequenceSpace.implicitReceiver().interval(arg1, arg2);
/*
udanax-top.st:43273:RequestHandler class methodsFor: 'translate: generated'!
{SequenceRegion} SequenceSpace.U.interval.U.N2: arg1 {Sequence} with: arg2 {Sequence}
	^SequenceSpace implicitReceiver interval: arg1 with: arg2!
*/
}
public static SequenceSpace SequenceSpaceUmakeUN0() {
	return SequenceSpace.make();
/*
udanax-top.st:43276:RequestHandler class methodsFor: 'translate: generated'!
{SequenceSpace} SequenceSpace.U.make.U.N0
	^SequenceSpace make!
*/
}
public static SequenceMapping SequenceSpaceUmappingUN1(PrimIntValue arg1) {
	return SequenceSpace.implicitReceiver().mapping(arg1.asIntegerVar());
/*
udanax-top.st:43279:RequestHandler class methodsFor: 'translate: generated'!
{SequenceMapping} SequenceSpace.U.mapping.U.N1: arg1 {PrimIntValue}
	^SequenceSpace implicitReceiver mapping: arg1 asIntegerVar!
*/
}
public static SequenceMapping SequenceSpaceUmappingUN2(PrimIntValue arg1, Sequence arg2) {
	return SequenceSpace.implicitReceiver().mapping(arg1.asIntegerVar(), arg2);
/*
udanax-top.st:43282:RequestHandler class methodsFor: 'translate: generated'!
{SequenceMapping} SequenceSpace.U.mapping.U.N2: arg1 {PrimIntValue} with: arg2 {Sequence}
	^SequenceSpace implicitReceiver mapping: arg1 asIntegerVar with: arg2!
*/
}
public static Sequence SequenceSpaceUpositionUN1(PrimArray arg1) {
	return SequenceSpace.implicitReceiver().position(arg1);
/*
udanax-top.st:43285:RequestHandler class methodsFor: 'translate: generated'!
{Sequence} SequenceSpace.U.position.U.N1: arg1 {PrimArray}
	^SequenceSpace implicitReceiver position: arg1!
*/
}
public static Sequence SequenceSpaceUpositionUN2(PrimArray arg1, PrimIntValue arg2) {
	return SequenceSpace.implicitReceiver().position(arg1, arg2.asIntegerVar());
/*
udanax-top.st:43288:RequestHandler class methodsFor: 'translate: generated'!
{Sequence} SequenceSpace.U.position.U.N2: arg1 {PrimArray} with: arg2 {PrimIntValue}
	^SequenceSpace implicitReceiver position: arg1 with: arg2 asIntegerVar!
*/
}
public static SequenceRegion SequenceSpaceUprefixedByUN2(Sequence arg1, PrimIntValue arg2) {
	return SequenceSpace.implicitReceiver().prefixedBy(arg1, arg2.asIntegerVar());
/*
udanax-top.st:43291:RequestHandler class methodsFor: 'translate: generated'!
{SequenceRegion} SequenceSpace.U.prefixedBy.U.N2: arg1 {Sequence} with: arg2 {PrimIntValue}
	^SequenceSpace implicitReceiver prefixedBy: arg1 with: arg2 asIntegerVar!
*/
}
public static ID ServerUaccessClubIDUN0() {
	return FeServer.accessClubID();
/*
udanax-top.st:43294:RequestHandler class methodsFor: 'translate: generated'!
{ID} Server.U.accessClubID.U.N0
	^FeServer accessClubID!
*/
}
public static ID ServerUadminClubIDUN0() {
	return FeServer.adminClubID();
/*
udanax-top.st:43297:RequestHandler class methodsFor: 'translate: generated'!
{ID} Server.U.adminClubID.U.N0
	^FeServer adminClubID!
*/
}
public static ID ServerUarchiveClubIDUN0() {
	return FeServer.archiveClubID();
/*
udanax-top.st:43300:RequestHandler class methodsFor: 'translate: generated'!
{ID} Server.U.archiveClubID.U.N0
	^FeServer archiveClubID!
*/
}
public static ID ServerUassignIDUN1(FeRangeElement arg1) {
	return FeServer.assignID(arg1);
/*
udanax-top.st:43303:RequestHandler class methodsFor: 'translate: generated'!
{ID} Server.U.assignID.U.N1: arg1 {FeRangeElement}
	^FeServer assignID: arg1!
*/
}
public static ID ServerUassignIDUN2(FeRangeElement arg1, ID arg2) {
	return FeServer.assignID(arg1, arg2);
/*
udanax-top.st:43306:RequestHandler class methodsFor: 'translate: generated'!
{ID} Server.U.assignID.U.N2: arg1 {FeRangeElement} with: arg2 {ID}
	^FeServer assignID: arg1 with: arg2!
*/
}
public static ID ServerUclubDirectoryIDUN0() {
	return FeServer.clubDirectoryID();
/*
udanax-top.st:43309:RequestHandler class methodsFor: 'translate: generated'!
{ID} Server.U.clubDirectoryID.U.N0
	^FeServer clubDirectoryID!
*/
}
public static PrimIntValue ServerUcurrentTimeUN0() {
	return PrimIntValue.make((FeServer.currentTime()));
/*
udanax-top.st:43312:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntValue} Server.U.currentTime.U.N0
	^PrimIntValue make: (FeServer currentTime)!
*/
}
public static ID ServerUemptyClubIDUN0() {
	return FeServer.emptyClubID();
/*
udanax-top.st:43315:RequestHandler class methodsFor: 'translate: generated'!
{ID} Server.U.emptyClubID.U.N0
	^FeServer emptyClubID!
*/
}
public static Sequence ServerUencrypterNameUN0() {
	return FeServer.encrypterName();
/*
udanax-top.st:43318:RequestHandler class methodsFor: 'translate: generated'!
{Sequence} Server.U.encrypterName.U.N0
	^FeServer encrypterName!
*/
}
public static FeRangeElement ServerUgetUN1(ID arg1) {
	return FeServer.get(arg1);
/*
udanax-top.st:43321:RequestHandler class methodsFor: 'translate: generated'!
{FeRangeElement} Server.U.get.U.N1: arg1 {ID}
	^FeServer get: arg1!
*/
}
public static Sequence ServerUidentifierUN0() {
	return FeServer.identifier();
/*
udanax-top.st:43324:RequestHandler class methodsFor: 'translate: generated'!
{Sequence} Server.U.identifier.U.N0
	^FeServer identifier!
*/
}
public static ID ServerUiDOfUN1(FeRangeElement arg1) {
	return FeServer.iDOf(arg1);
/*
udanax-top.st:43327:RequestHandler class methodsFor: 'translate: generated'!
{ID} Server.U.iDOf.U.N1: arg1 {FeRangeElement}
	^FeServer iDOf: arg1!
*/
}
public static IDRegion ServerUiDsOfUN1(FeRangeElement arg1) {
	return FeServer.iDsOf(arg1);
/*
udanax-top.st:43330:RequestHandler class methodsFor: 'translate: generated'!
{IDRegion} Server.U.iDsOf.U.N1: arg1 {FeRangeElement}
	^FeServer iDsOf: arg1!
*/
}
public static IDRegion ServerUiDsOfRangeUN1(FeEdition arg1) {
	return FeServer.iDsOfRange(arg1);
/*
udanax-top.st:43333:RequestHandler class methodsFor: 'translate: generated'!
{IDRegion} Server.U.iDsOfRange.U.N1: arg1 {FeEdition}
	^FeServer iDsOfRange: arg1!
*/
}
public static Lock ServerUloginUN1(ID arg1) {
	return FeServer.login(arg1);
/*
udanax-top.st:43336:RequestHandler class methodsFor: 'translate: generated'!
{Lock} Server.U.login.U.N1: arg1 {ID}
	^FeServer login: arg1!
*/
}
public static Lock ServerUloginByNameUN1(Sequence arg1) {
	return FeServer.loginByName(arg1);
/*
udanax-top.st:43339:RequestHandler class methodsFor: 'translate: generated'!
{Lock} Server.U.loginByName.U.N1: arg1 {Sequence}
	^FeServer loginByName: arg1!
*/
}
public static ID ServerUpublicClubIDUN0() {
	return FeServer.publicClubID();
/*
udanax-top.st:43342:RequestHandler class methodsFor: 'translate: generated'!
{ID} Server.U.publicClubID.U.N0
	^FeServer publicClubID!
*/
}
public static PrimIntArray ServerUpublicKeyUN0() {
	return FeServer.publicKey();
/*
udanax-top.st:43345:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntArray} Server.U.publicKey.U.N0
	^FeServer publicKey!
*/
}
public static PrimIntValue SessionUconnectTimeUN1(FeSession receiver) {
	return PrimIntValue.make((receiver.connectTime()));
/*
udanax-top.st:43348:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntValue} Session.U.connectTime.U.N1: receiver {FeSession}
	^PrimIntValue make: (receiver connectTime)!
*/
}
public static FeSession SessionUcurrentUN0() {
	return FeSession.current();
/*
udanax-top.st:43351:RequestHandler class methodsFor: 'translate: generated'!
{FeSession} Session.U.current.U.N0
	^FeSession current!
*/
}
public static void SessionUendSessionUN1(FeSession receiver) {
	receiver.endSession();
/*
udanax-top.st:43354:RequestHandler class methodsFor: 'translate: generated'!
{void} Session.U.endSession.U.N1: receiver {FeSession}
	receiver endSession!
*/
}
public static void SessionUendSessionUN2(FeSession receiver, boolean arg1) {
	receiver.endSession(arg1);
/*
udanax-top.st:43357:RequestHandler class methodsFor: 'translate: generated'!
{void} Session.U.endSession.U.N2: receiver {FeSession} with: arg1 {BooleanVar}
	receiver endSession: arg1!
*/
}
public static ID SessionUinitialLoginUN1(FeSession receiver) {
	return receiver.initialLogin();
/*
udanax-top.st:43360:RequestHandler class methodsFor: 'translate: generated'!
{ID} Session.U.initialLogin.U.N1: receiver {FeSession}
	^receiver initialLogin!
*/
}
public static boolean SessionUisConnectedUN1(FeSession receiver) {
	return receiver.isConnected();
/*
udanax-top.st:43363:RequestHandler class methodsFor: 'translate: generated'!
{BooleanVar} Session.U.isConnected.U.N1: receiver {FeSession}
	^receiver isConnected!
*/
}
public static PrimIntArray SessionUportUN1(FeSession receiver) {
	return receiver.port();
/*
udanax-top.st:43366:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntArray} Session.U.port.U.N1: receiver {FeSession}
	^receiver port!
*/
}
public static PrimIntValue SetUcountUN1(FeSet receiver) {
	return PrimIntValue.make((receiver.count()));
/*
udanax-top.st:43369:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntValue} Set.U.count.U.N1: receiver {FeSet}
	^PrimIntValue make: (receiver count)!
*/
}
public static boolean SetUincludesUN2(FeSet receiver, FeRangeElement arg1) {
	return receiver.includes(arg1);
/*
udanax-top.st:43372:RequestHandler class methodsFor: 'translate: generated'!
{BooleanVar} Set.U.includes.U.N2: receiver {FeSet} with: arg1 {FeRangeElement}
	^receiver includes: arg1!
*/
}
public static FeSet SetUintersectUN2(FeSet receiver, FeSet arg1) {
	return receiver.intersect(arg1);
/*
udanax-top.st:43375:RequestHandler class methodsFor: 'translate: generated'!
{FeSet} Set.U.intersect.U.N2: receiver {FeSet} with: arg1 {FeSet}
	^receiver intersect: arg1!
*/
}
public static FeSet SetUmakeUN0() {
	return FeSet.make();
/*
udanax-top.st:43378:RequestHandler class methodsFor: 'translate: generated'!
{FeSet} Set.U.make.U.N0
	^FeSet make!
*/
}
public static FeSet SetUmakeUN1(PtrArray arg1) {
	return FeSet.make(arg1);
/*
udanax-top.st:43381:RequestHandler class methodsFor: 'translate: generated'!
{FeSet} Set.U.make.U.N1: arg1 {PtrArray}
	^FeSet make: arg1!
*/
}
public static FeSet SetUminusUN2(FeSet receiver, FeSet arg1) {
	return receiver.minus(arg1);
/*
udanax-top.st:43384:RequestHandler class methodsFor: 'translate: generated'!
{FeSet} Set.U.minus.U.N2: receiver {FeSet} with: arg1 {FeSet}
	^receiver minus: arg1!
*/
}
public static FeRangeElement SetUtheOneUN1(FeSet receiver) {
	return receiver.theOne();
/*
udanax-top.st:43387:RequestHandler class methodsFor: 'translate: generated'!
{FeRangeElement} Set.U.theOne.U.N1: receiver {FeSet}
	^receiver theOne!
*/
}
public static FeSet SetUunionWithUN2(FeSet receiver, FeSet arg1) {
	return receiver.unionWith(arg1);
/*
udanax-top.st:43390:RequestHandler class methodsFor: 'translate: generated'!
{FeSet} Set.U.unionWith.U.N2: receiver {FeSet} with: arg1 {FeSet}
	^receiver unionWith: arg1!
*/
}
public static FeSet SetUwithUN2(FeSet receiver, FeRangeElement arg1) {
	return receiver.with(arg1);
/*
udanax-top.st:43393:RequestHandler class methodsFor: 'translate: generated'!
{FeSet} Set.U.with.U.N2: receiver {FeSet} with: arg1 {FeRangeElement}
	^receiver with: arg1!
*/
}
public static FeSet SetUwithoutUN2(FeSet receiver, FeRangeElement arg1) {
	return receiver.without(arg1);
/*
udanax-top.st:43396:RequestHandler class methodsFor: 'translate: generated'!
{FeSet} Set.U.without.U.N2: receiver {FeSet} with: arg1 {FeRangeElement}
	^receiver without: arg1!
*/
}
public static FeEdition SingleRefUexcerptUN1(FeSingleRef receiver) {
	return receiver.excerpt();
/*
udanax-top.st:43399:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} SingleRef.U.excerpt.U.N1: receiver {FeSingleRef}
	^receiver excerpt!
*/
}
public static FeSingleRef SingleRefUmakeUN1(FeEdition arg1) {
	return FeSingleRef.make(arg1);
/*
udanax-top.st:43402:RequestHandler class methodsFor: 'translate: generated'!
{FeSingleRef} SingleRef.U.make.U.N1: arg1 {FeEdition}
	^FeSingleRef make: arg1!
*/
}
public static FeSingleRef SingleRefUmakeUN2(FeEdition arg1, FeWork arg2) {
	return FeSingleRef.make(arg1, arg2);
/*
udanax-top.st:43405:RequestHandler class methodsFor: 'translate: generated'!
{FeSingleRef} SingleRef.U.make.U.N2: arg1 {FeEdition} with: arg2 {FeWork}
	^FeSingleRef make: arg1 with: arg2!
*/
}
public static FeSingleRef SingleRefUmakeUN3(FeEdition arg1, FeWork arg2, FeWork arg3) {
	return FeSingleRef.make(arg1, arg2, arg3);
/*
udanax-top.st:43408:RequestHandler class methodsFor: 'translate: generated'!
{FeSingleRef} SingleRef.U.make.U.N3: arg1 {FeEdition} with: arg2 {FeWork} with: arg3 {FeWork}
	^FeSingleRef make: arg1 with: arg2 with: arg3!
*/
}
public static FeSingleRef SingleRefUmakeUN4(FeEdition arg1, FeWork arg2, FeWork arg3, FePath arg4) {
	return FeSingleRef.make(arg1, arg2, arg3, arg4);
/*
udanax-top.st:43411:RequestHandler class methodsFor: 'translate: generated'!
{FeSingleRef} SingleRef.U.make.U.N4: arg1 {FeEdition} with: arg2 {FeWork} with: arg3 {FeWork} with: arg4 {FePath}
	^FeSingleRef make: arg1 with: arg2 with: arg3 with: arg4!
*/
}
public static FeSingleRef SingleRefUwithExcerptUN2(FeSingleRef receiver, FeEdition arg1) {
	return receiver.withExcerpt(arg1);
/*
udanax-top.st:43414:RequestHandler class methodsFor: 'translate: generated'!
{FeSingleRef} SingleRef.U.withExcerpt.U.N2: receiver {FeSingleRef} with: arg1 {FeEdition}
	^receiver withExcerpt: arg1!
*/
}
public static boolean StepperUatEndUN1(Stepper receiver) {
	return receiver.end();
/*
udanax-top.st:43417:RequestHandler class methodsFor: 'translate: generated'!
{BooleanVar} Stepper.U.atEnd.U.N1: receiver {Stepper}
	^receiver atEnd!
*/
}
public static Stepper StepperUcopyUN1(Stepper receiver) {
	return receiver.copy();
/*
udanax-top.st:43420:RequestHandler class methodsFor: 'translate: generated'!
{Stepper} Stepper.U.copy.U.N1: receiver {Stepper}
	^receiver copy!
*/
}
public static Heaper StepperUgetUN1(Stepper receiver) {
	return receiver.get();
/*
udanax-top.st:43423:RequestHandler class methodsFor: 'translate: generated'!
{Heaper} Stepper.U.get.U.N1: receiver {Stepper}
	^receiver get!
*/
}
public static void StepperUstepUN1(Stepper receiver) {
	receiver.step();
/*
udanax-top.st:43426:RequestHandler class methodsFor: 'translate: generated'!
{void} Stepper.U.step.U.N1: receiver {Stepper}
	receiver step!
*/
}
public static PrimArray StepperUstepManyUN1(Stepper receiver) {
	return receiver.stepMany();
/*
udanax-top.st:43429:RequestHandler class methodsFor: 'translate: generated'!
{PrimArray} Stepper.U.stepMany.U.N1: receiver {Stepper}
	^receiver stepMany!
*/
}
public static PrimArray StepperUstepManyUN2(Stepper receiver, PrimIntValue arg1) {
	return receiver.stepMany(arg1.asInt32());
/*
udanax-top.st:43432:RequestHandler class methodsFor: 'translate: generated'!
{PrimArray} Stepper.U.stepMany.U.N2: receiver {Stepper} with: arg1 {PrimIntValue}
	^receiver stepMany: arg1 asInt32!
*/
}
public static Heaper StepperUtheOneUN1(Stepper receiver) {
	return receiver.theOne();
/*
udanax-top.st:43435:RequestHandler class methodsFor: 'translate: generated'!
{Heaper} Stepper.U.theOne.U.N1: receiver {Stepper}
	^receiver theOne!
*/
}
public static Position TableStepperUpositionUN1(TableStepper receiver) {
	return receiver.position();
/*
udanax-top.st:43438:RequestHandler class methodsFor: 'translate: generated'!
{Position} TableStepper.U.position.U.N1: receiver {TableStepper}
	^receiver position!
*/
}
public static PrimArray TableStepperUstepManyPairsUN1(TableStepper receiver) {
	return receiver.stepManyPairs();
/*
udanax-top.st:43441:RequestHandler class methodsFor: 'translate: generated'!
{PrimArray} TableStepper.U.stepManyPairs.U.N1: receiver {TableStepper}
	^receiver stepManyPairs!
*/
}
public static PrimArray TableStepperUstepManyPairsUN2(TableStepper receiver, PrimIntValue arg1) {
	return receiver.stepManyPairs(arg1.asInt32());
/*
udanax-top.st:43444:RequestHandler class methodsFor: 'translate: generated'!
{PrimArray} TableStepper.U.stepManyPairs.U.N2: receiver {TableStepper} with: arg1 {PrimIntValue}
	^receiver stepManyPairs: arg1 asInt32!
*/
}
public static FeEdition TextUcontentsUN1(FeText receiver) {
	return receiver.contents();
/*
udanax-top.st:43447:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} Text.U.contents.U.N1: receiver {FeText}
	^receiver contents!
*/
}
public static PrimIntValue TextUcountUN1(FeText receiver) {
	return PrimIntValue.make((receiver.count()));
/*
udanax-top.st:43450:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntValue} Text.U.count.U.N1: receiver {FeText}
	^PrimIntValue make: (receiver count)!
*/
}
public static FeText TextUextractUN2(FeText receiver, IntegerRegion arg1) {
	return receiver.extract(arg1);
/*
udanax-top.st:43453:RequestHandler class methodsFor: 'translate: generated'!
{FeText} Text.U.extract.U.N2: receiver {FeText} with: arg1 {IntegerRegion}
	^receiver extract: arg1!
*/
}
public static FeText TextUinsertUN3(FeText receiver, PrimIntValue arg1, FeText arg2) {
	return receiver.insert(arg1.asIntegerVar(), arg2);
/*
udanax-top.st:43456:RequestHandler class methodsFor: 'translate: generated'!
{FeText} Text.U.insert.U.N3: receiver {FeText} with: arg1 {PrimIntValue} with: arg2 {FeText}
	^receiver insert: arg1 asIntegerVar with: arg2!
*/
}
public static FeText TextUmakeUN1(PrimArray arg1) {
	return FeText.make(arg1);
/*
udanax-top.st:43459:RequestHandler class methodsFor: 'translate: generated'!
{FeText} Text.U.make.U.N1: arg1 {PrimArray}
	^FeText make: arg1!
*/
}
public static FeText TextUmoveUN3(FeText receiver, PrimIntValue arg1, IntegerRegion arg2) {
	return receiver.move(arg1.asIntegerVar(), arg2);
/*
udanax-top.st:43462:RequestHandler class methodsFor: 'translate: generated'!
{FeText} Text.U.move.U.N3: receiver {FeText} with: arg1 {PrimIntValue} with: arg2 {IntegerRegion}
	^receiver move: arg1 asIntegerVar with: arg2!
*/
}
public static FeText TextUreplaceUN3(FeText receiver, IntegerRegion arg1, FeText arg2) {
	return receiver.replace(arg1, arg2);
/*
udanax-top.st:43465:RequestHandler class methodsFor: 'translate: generated'!
{FeText} Text.U.replace.U.N3: receiver {FeText} with: arg1 {IntegerRegion} with: arg2 {FeText}
	^receiver replace: arg1 with: arg2!
*/
}
public static Position TupleUcoordinateUN2(Tuple receiver, PrimIntValue arg1) {
	return receiver.coordinate(arg1.asInt32());
/*
udanax-top.st:43468:RequestHandler class methodsFor: 'translate: generated'!
{Position} Tuple.U.coordinate.U.N2: receiver {Tuple} with: arg1 {PrimIntValue}
	^receiver coordinate: arg1 asInt32!
*/
}
public static PtrArray TupleUcoordinatesUN1(Tuple receiver) {
	return receiver.coordinates();
/*
udanax-top.st:43471:RequestHandler class methodsFor: 'translate: generated'!
{PtrArray} Tuple.U.coordinates.U.N1: receiver {Tuple}
	^receiver coordinates!
*/
}
public static FeWallLockSmith WallLockSmithUmakeUN0() {
	return FeWallLockSmith.make();
/*
udanax-top.st:43474:RequestHandler class methodsFor: 'translate: generated'!
{FeWallLockSmith} WallLockSmith.U.make.U.N0
	^FeWallLockSmith make!
*/
}
public static boolean WorkUcanReadUN1(FeWork receiver) {
	return receiver.canRead();
/*
udanax-top.st:43477:RequestHandler class methodsFor: 'translate: generated'!
{BooleanVar} Work.U.canRead.U.N1: receiver {FeWork}
	^receiver canRead!
*/
}
public static boolean WorkUcanReviseUN1(FeWork receiver) {
	return receiver.canRevise();
/*
udanax-top.st:43480:RequestHandler class methodsFor: 'translate: generated'!
{BooleanVar} Work.U.canRevise.U.N1: receiver {FeWork}
	^receiver canRevise!
*/
}
public static ID WorkUeditClubUN1(FeWork receiver) {
	return receiver.editClub();
/*
udanax-top.st:43483:RequestHandler class methodsFor: 'translate: generated'!
{ID} Work.U.editClub.U.N1: receiver {FeWork}
	^receiver editClub!
*/
}
public static FeEdition WorkUeditionUN1(FeWork receiver) {
	return receiver.edition();
/*
udanax-top.st:43486:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} Work.U.edition.U.N1: receiver {FeWork}
	^receiver edition!
*/
}
public static void WorkUendorseUN2(FeWork receiver, CrossRegion arg1) {
	receiver.endorse(arg1);
/*
udanax-top.st:43489:RequestHandler class methodsFor: 'translate: generated'!
{void} Work.U.endorse.U.N2: receiver {FeWork} with: arg1 {CrossRegion}
	receiver endorse: arg1!
*/
}
public static CrossRegion WorkUendorsementsUN1(FeWork receiver) {
	return receiver.endorsements();
/*
udanax-top.st:43492:RequestHandler class methodsFor: 'translate: generated'!
{CrossRegion} Work.U.endorsements.U.N1: receiver {FeWork}
	^receiver endorsements!
*/
}
public static void WorkUgrabUN1(FeWork receiver) {
	receiver.grab();
/*
udanax-top.st:43495:RequestHandler class methodsFor: 'translate: generated'!
{void} Work.U.grab.U.N1: receiver {FeWork}
	receiver grab!
*/
}
public static ID WorkUgrabberUN1(FeWork receiver) {
	return receiver.grabber();
/*
udanax-top.st:43498:RequestHandler class methodsFor: 'translate: generated'!
{ID} Work.U.grabber.U.N1: receiver {FeWork}
	^receiver grabber!
*/
}
public static ID WorkUhistoryClubUN1(FeWork receiver) {
	return receiver.historyClub();
/*
udanax-top.st:43501:RequestHandler class methodsFor: 'translate: generated'!
{ID} Work.U.historyClub.U.N1: receiver {FeWork}
	^receiver historyClub!
*/
}
public static ID WorkUlastRevisionAuthorUN1(FeWork receiver) {
	return receiver.lastRevisionAuthor();
/*
udanax-top.st:43504:RequestHandler class methodsFor: 'translate: generated'!
{ID} Work.U.lastRevisionAuthor.U.N1: receiver {FeWork}
	^receiver lastRevisionAuthor!
*/
}
public static PrimIntValue WorkUlastRevisionNumberUN1(FeWork receiver) {
	return PrimIntValue.make((receiver.lastRevisionNumber()));
/*
udanax-top.st:43507:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntValue} Work.U.lastRevisionNumber.U.N1: receiver {FeWork}
	^PrimIntValue make: (receiver lastRevisionNumber)!
*/
}
public static PrimIntValue WorkUlastRevisionTimeUN1(FeWork receiver) {
	return PrimIntValue.make((receiver.lastRevisionTime()));
/*
udanax-top.st:43510:RequestHandler class methodsFor: 'translate: generated'!
{PrimIntValue} Work.U.lastRevisionTime.U.N1: receiver {FeWork}
	^PrimIntValue make: (receiver lastRevisionTime)!
*/
}
public static FeWork WorkUmakeUN1(FeEdition arg1) {
	return FeWork.make(arg1);
/*
udanax-top.st:43513:RequestHandler class methodsFor: 'translate: generated'!
{FeWork} Work.U.make.U.N1: arg1 {FeEdition}
	^FeWork make: arg1!
*/
}
public static ID WorkUreadClubUN1(FeWork receiver) {
	return receiver.readClub();
/*
udanax-top.st:43516:RequestHandler class methodsFor: 'translate: generated'!
{ID} Work.U.readClub.U.N1: receiver {FeWork}
	^receiver readClub!
*/
}
public static void WorkUreleaseUN1(FeWork receiver) {
	receiver.release();
/*
udanax-top.st:43519:RequestHandler class methodsFor: 'translate: generated'!
{void} Work.U.release.U.N1: receiver {FeWork}
	receiver release!
*/
}
public static void WorkUremoveEditClubUN1(FeWork receiver) {
	receiver.removeEditClub();
/*
udanax-top.st:43522:RequestHandler class methodsFor: 'translate: generated'!
{void} Work.U.removeEditClub.U.N1: receiver {FeWork}
	receiver removeEditClub!
*/
}
public static void WorkUremoveReadClubUN1(FeWork receiver) {
	receiver.removeReadClub();
/*
udanax-top.st:43525:RequestHandler class methodsFor: 'translate: generated'!
{void} Work.U.removeReadClub.U.N1: receiver {FeWork}
	receiver removeReadClub!
*/
}
public static void WorkUrequestGrabUN1(FeWork receiver) {
	receiver.requestGrab();
/*
udanax-top.st:43528:RequestHandler class methodsFor: 'translate: generated'!
{void} Work.U.requestGrab.U.N1: receiver {FeWork}
	receiver requestGrab!
*/
}
public static void WorkUretractUN2(FeWork receiver, CrossRegion arg1) {
	receiver.retract(arg1);
/*
udanax-top.st:43531:RequestHandler class methodsFor: 'translate: generated'!
{void} Work.U.retract.U.N2: receiver {FeWork} with: arg1 {CrossRegion}
	receiver retract: arg1!
*/
}
public static void WorkUreviseUN2(FeWork receiver, FeEdition arg1) {
	receiver.revise(arg1);
/*
udanax-top.st:43534:RequestHandler class methodsFor: 'translate: generated'!
{void} Work.U.revise.U.N2: receiver {FeWork} with: arg1 {FeEdition}
	receiver revise: arg1!
*/
}
public static FeEdition WorkUrevisionsUN1(FeWork receiver) {
	return receiver.revisions();
/*
udanax-top.st:43537:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} Work.U.revisions.U.N1: receiver {FeWork}
	^receiver revisions!
*/
}
public static void WorkUsetEditClubUN2(FeWork receiver, ID arg1) {
	receiver.setEditClub(arg1);
/*
udanax-top.st:43540:RequestHandler class methodsFor: 'translate: generated'!
{void} Work.U.setEditClub.U.N2: receiver {FeWork} with: arg1 {ID}
	receiver setEditClub: arg1!
*/
}
public static void WorkUsetHistoryClubUN2(FeWork receiver, ID arg1) {
	receiver.setHistoryClub(arg1);
/*
udanax-top.st:43543:RequestHandler class methodsFor: 'translate: generated'!
{void} Work.U.setHistoryClub.U.N2: receiver {FeWork} with: arg1 {ID}
	receiver setHistoryClub: arg1!
*/
}
public static void WorkUsetReadClubUN2(FeWork receiver, ID arg1) {
	receiver.setReadClub(arg1);
/*
udanax-top.st:43546:RequestHandler class methodsFor: 'translate: generated'!
{void} Work.U.setReadClub.U.N2: receiver {FeWork} with: arg1 {ID}
	receiver setReadClub: arg1!
*/
}
public static void WorkUsponsorUN2(FeWork receiver, IDRegion arg1) {
	receiver.sponsor(arg1);
/*
udanax-top.st:43549:RequestHandler class methodsFor: 'translate: generated'!
{void} Work.U.sponsor.U.N2: receiver {FeWork} with: arg1 {IDRegion}
	receiver sponsor: arg1!
*/
}
public static IDRegion WorkUsponsorsUN1(FeWork receiver) {
	return receiver.sponsors();
/*
udanax-top.st:43552:RequestHandler class methodsFor: 'translate: generated'!
{IDRegion} Work.U.sponsors.U.N1: receiver {FeWork}
	^receiver sponsors!
*/
}
public static void WorkUunsponsorUN2(FeWork receiver, IDRegion arg1) {
	receiver.unsponsor(arg1);
/*
udanax-top.st:43555:RequestHandler class methodsFor: 'translate: generated'!
{void} Work.U.unsponsor.U.N2: receiver {FeWork} with: arg1 {IDRegion}
	receiver unsponsor: arg1!
*/
}
public static FeEdition WrapperUeditionUN1(FeWrapper receiver) {
	return receiver.edition();
/*
udanax-top.st:43558:RequestHandler class methodsFor: 'translate: generated'!
{FeEdition} Wrapper.U.edition.U.N1: receiver {FeWrapper}
	^receiver edition!
*/
}
public static FeWrapper WrapperUinnerUN1(FeWrapper receiver) {
	return receiver.inner();
/*
udanax-top.st:43561:RequestHandler class methodsFor: 'translate: generated'!
{FeWrapper} Wrapper.U.inner.U.N1: receiver {FeWrapper}
	^receiver inner!
*/
}
public static Filter WrapperSpecUfilterUN1(FeWrapperSpec receiver) {
	return receiver.filter();
/*
udanax-top.st:43564:RequestHandler class methodsFor: 'translate: generated'!
{Filter} WrapperSpec.U.filter.U.N1: receiver {FeWrapperSpec}
	^receiver filter!
*/
}
public static FeWrapperSpec WrapperSpecUgetUN1(Sequence arg1) {
	return FeWrapperSpec.get(arg1);
/*
udanax-top.st:43567:RequestHandler class methodsFor: 'translate: generated'!
{FeWrapperSpec} WrapperSpec.U.get.U.N1: arg1 {Sequence}
	^FeWrapperSpec get: arg1!
*/
}
public static Sequence WrapperSpecUnameUN1(FeWrapperSpec receiver) {
	return receiver.name();
/*
udanax-top.st:43570:RequestHandler class methodsFor: 'translate: generated'!
{Sequence} WrapperSpec.U.name.U.N1: receiver {FeWrapperSpec}
	^receiver name!
*/
}
public static FeWrapper WrapperSpecUwrapUN2(FeWrapperSpec receiver, FeEdition arg1) {
	return receiver.wrap(arg1);
/*
udanax-top.st:43573:RequestHandler class methodsFor: 'translate: generated'!
{FeWrapper} WrapperSpec.U.wrap.U.N2: receiver {FeWrapperSpec} with: arg1 {FeEdition}
	^receiver wrap: arg1!
*/
}
public RequestHandler() {
/*

Generated during transformation
*/
}
public RequestHandler(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
public Fn instVarAt(int i) {
	throw new UnsupportedOperationException();
/*

Generated during transformation: AddMethod
*/
}
}
