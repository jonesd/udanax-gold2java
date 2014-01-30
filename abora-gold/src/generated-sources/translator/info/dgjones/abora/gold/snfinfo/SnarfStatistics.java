/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.snfinfo;

import info.dgjones.abora.gold.cobbler.Cookbook;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.fm.support.Thunk;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.java.urdi.Urdi;
import info.dgjones.abora.gold.java.urdi.UrdiView;
import info.dgjones.abora.gold.negoti8.ProtocolBroker;
import info.dgjones.abora.gold.primtab.PrimIndexTable;
import info.dgjones.abora.gold.primtab.PrimPtr2PtrTable;
import info.dgjones.abora.gold.snarf.Abraham;
import info.dgjones.abora.gold.snarf.SnarfHandler;
import info.dgjones.abora.gold.snfinfo.SnarfStatistics;
import info.dgjones.abora.gold.snfinfo.SpecialistRcvrJig;
import info.dgjones.abora.gold.urdi.SnarfInfoHandler;
import info.dgjones.abora.gold.xcvr.DiskSpecialist;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.SpecialistRcvr;
import info.dgjones.abora.gold.xcvr.TextyXcvrMaker;
import info.dgjones.abora.gold.xcvr.XcvrMaker;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xcvr.XnReadStream;
import info.dgjones.abora.gold.xpp.basic.Category;

/**
 * Print out some summary of the data currently on disk.
 */
public class SnarfStatistics extends Thunk {

	protected String myFilename;
	protected Cookbook myCookbook;
	protected XcvrMaker myProtocol;
/*
udanax-top.st:57810:
Thunk subclass: #SnarfStatistics
	instanceVariableNames: '
		myFilename {char star}
		myCookbook {Cookbook NOCOPY}
		myProtocol {XcvrMaker NOCOPY}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-snfinfo'!
*/
/*
udanax-top.st:57817:
SnarfStatistics comment:
'Print out some summary of the data currently on disk.'!
*/
/*
udanax-top.st:57819:
(SnarfStatistics getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(SnarfStatistics.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}).add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public void execute() {
	snarfAllocInfo();
	tallyFlockTypes();
/*
udanax-top.st:57824:SnarfStatistics methodsFor: 'running'!
{void} execute
	self snarfAllocInfo.
	self tallyFlockTypes.!
*/
}
public void snarfAllocInfo() {
	Urdi anUrdi;
	UrdiView view;
	SnarfInfoHandler info;
	int totalReal;
	int totalForget;
	int totalRealSpace;
	int totalForgetSpace;
	totalReal = 0;
	totalForget = 0;
	totalRealSpace = 0;
	totalForgetSpace = 0;
	anUrdi = (Urdi.urdi(myFilename, 2));
	view = anUrdi.makeReadView();
	info = SnarfInfoHandler.make(anUrdi, view);
	AboraSupport.logger.print("There are ");
	AboraSupport.logger.print(info.snarfInfoCount());
	AboraSupport.logger.print(" snarFInfo snarfs out of ");
	AboraSupport.logger.print(info.snarfCount());
	AboraSupport.logger.print(" total snarfs.\n"+
"There are ");
	AboraSupport.logger.print((view.getDataSizeOfSnarf(1)));
	AboraSupport.logger.print(" bytes in each snarf.\n"+
"");
	for (int snarfID = info.snarfInfoCount(); snarfID < info.snarfCount(); snarfID ++ ) {
		if ((info.getSpaceLeft(snarfID)) < (view.getDataSizeOfSnarf(snarfID))) {
			SnarfHandler handler;
			int count;
			int forwards;
			int forgets;
			int forgetSpace;
			int flocks;
			int liveSpace;
			handler = SnarfHandler.make((view.makeReadHandle(snarfID)));
			count = handler.mapCount();
			forwards = forgets = forgetSpace = flocks = liveSpace = 0;
			for (int i = 0; i < count; i ++ ) {
				if (handler.isOccupied(i)) {
					if ((handler.fetchForward(i)) != null) {
						forwards = forwards + 1;
					}
					else {
						if (handler.isForgotten(i)) {
							forgets = forgets + 1;
							forgetSpace = forgetSpace + (handler.flockSize(i));
						}
						else {
							flocks = flocks + 1;
							liveSpace = liveSpace + (handler.flockSize(i));
						}
					}
				}
			}
			AboraSupport.logger.print(snarfID);
			AboraSupport.logger.print(":	");
			AboraSupport.logger.print(flocks);
			AboraSupport.logger.print(" real in ");
			AboraSupport.logger.print(liveSpace);
			AboraSupport.logger.print(" bytes.	");
			AboraSupport.logger.print(forgets);
			AboraSupport.logger.print(" forgets in ");
			AboraSupport.logger.print(forgetSpace);
			AboraSupport.logger.print(" bytes.	");
			AboraSupport.logger.print(forwards);
			AboraSupport.logger.print(" forward.	");
			AboraSupport.logger.print(handler.spaceLeft());
			AboraSupport.logger.print(" spaceLeft.");
			AboraSupport.logger.print("	forgotten: ");
			AboraSupport.logger.print((info.getForgottenFlag(snarfID)));
			AboraSupport.logger.print(".\n"+
"");
			handler.destroy();
			totalReal = totalReal + flocks;
			totalRealSpace = totalRealSpace + liveSpace;
			totalForget = totalForget + forgets;
			totalForgetSpace = totalForgetSpace + forgetSpace;
		}
	}
	AboraSupport.logger.print("All others empty.\n"+
"");
	AboraSupport.logger.print("Totals:  ");
	AboraSupport.logger.print(totalReal);
	AboraSupport.logger.print(" real in ");
	AboraSupport.logger.print(totalRealSpace);
	AboraSupport.logger.print(" bytes, ");
	AboraSupport.logger.print(totalForget);
	AboraSupport.logger.print(" forgets in ");
	AboraSupport.logger.print(totalForgetSpace);
	AboraSupport.logger.print(" bytes.\n"+
"");
	info.destroy();
	view.destroy();
	anUrdi.destroy();
/*
udanax-top.st:57828:SnarfStatistics methodsFor: 'running'!
{void} snarfAllocInfo
	| anUrdi {Urdi} view {UrdiView} info {SnarfInfoHandler} 
	totalReal {IntegerVar} totalForget {IntegerVar}
	totalRealSpace {IntegerVar} totalForgetSpace {IntegerVar} |
	totalReal _ IntegerVarZero.
	totalForget _ IntegerVarZero.
	totalRealSpace _ IntegerVarZero.
	totalForgetSpace _ IntegerVarZero.
	anUrdi _ (Urdi urdi: myFilename with: 2).
	view _ anUrdi makeReadView.
	info _ SnarfInfoHandler make: anUrdi with: view.
cerr << 'There are ' << info snarfInfoCount << ' snarFInfo snarfs out of ' << info snarfCount << ' total snarfs.
There are ' << (view getDataSizeOfSnarf: 1) << ' bytes in each snarf.
'.
	info snarfInfoCount almostTo: info snarfCount do: [:snarfID {Int32} |
		(info getSpaceLeft: snarfID) < (view getDataSizeOfSnarf: snarfID)
			ifTrue:
				[| handler {SnarfHandler} count {Int32} forwards {Int32} forgets {Int32} forgetSpace {Int32} flocks {Int32} liveSpace {Int32} |
				handler _ SnarfHandler make: (view makeReadHandle: snarfID).
				count _ handler mapCount.
				forwards _ forgets _ forgetSpace _ flocks _ liveSpace _ Int32Zero.
				Int32Zero almostTo: count do: [:i {Int32} |
					(handler isOccupied: i) ifTrue:
						[(handler fetchForward: i) ~~ NULL ifTrue: [forwards _ forwards + 1]
						ifFalse: [(handler isForgotten: i) ifTrue: [forgets _ forgets + 1.  
												forgetSpace _ forgetSpace + (handler flockSize: i)]
							ifFalse: [flocks _ flocks + 1.  liveSpace _ liveSpace + (handler flockSize: i)]]]].
				cerr << snarfID << ':	' << flocks << ' real in ' << liveSpace << ' bytes.	'.
				cerr << forgets << ' forgets in ' << forgetSpace << ' bytes.	'.
				cerr << forwards << ' forward.	'.
				cerr "<< count << ' cells '" << handler spaceLeft << ' spaceLeft.'.
				cerr << '	forgotten: ' << (info getForgottenFlag: snarfID) << '.
'.
				handler destroy.
				totalReal _ totalReal + flocks.
				totalRealSpace _ totalRealSpace + liveSpace.
				totalForget _ totalForget + forgets.
				totalForgetSpace _ totalForgetSpace + forgetSpace]].
	cerr << 'All others empty.
'.
	cerr << 'Totals:  ' << totalReal << ' real in ' << totalRealSpace << ' bytes, '
		 << totalForget << ' forgets in ' << totalForgetSpace << ' bytes.
'.
	info destroy.
	view destroy.
	anUrdi destroy!
*/
}
public void tallyFlockTypes() {
	Urdi anUrdi;
	UrdiView view;
	SnarfInfoHandler info;
	PrimIndexTable liveFlockCounts;
	PrimPtr2PtrTable liveFlockTypes;
	PrimIndexTable forgottenFlockCounts;
	PrimPtr2PtrTable forgottenFlockTypes;
	liveFlockCounts = PrimIndexTable.make(255);
	liveFlockTypes = PrimPtr2PtrTable.make(255);
	forgottenFlockCounts = PrimIndexTable.make(255);
	forgottenFlockTypes = PrimPtr2PtrTable.make(255);
	anUrdi = Urdi.urdi(myFilename, 2);
	view = anUrdi.makeReadView();
	info = SnarfInfoHandler.make(anUrdi, view);
	diskCookbook(view, info);
	AboraSupport.logger.print("Tallying types over all snarfs, this may take a while.\n"+
"");
	for (int snarfID = info.snarfInfoCount(); snarfID < info.snarfCount(); snarfID ++ ) {
		if ((info.getSpaceLeft(snarfID)) < (view.getDataSizeOfSnarf(snarfID))) {
			SnarfHandler handler;
			int count;
			handler = SnarfHandler.make((view.makeReadHandle(snarfID)));
			count = handler.mapCount();
			for (int i = 0; i < count; i ++ ) {
				if ((handler.isOccupied(i)) && ((handler.fetchForward(i)) == null)) {
					Rcvr rcvr;
					XnReadStream stream;
					Category cat;
					rcvr = makeRcvr((stream = handler.readStream(i)));
					cat = SpecialistRcvrJig.receiveCategory(rcvr);
					rcvr.destroy();
					stream.destroy();
					if ( ! (cat.isEqualOrSubclassOf(AboraSupport.findCategory(Abraham.class)))) {
						AboraSupport.logger.print("WARNING: non-Abraham flock at ");
						AboraSupport.logger.print(snarfID);
						AboraSupport.logger.print(":");
						AboraSupport.logger.print(i);
						AboraSupport.logger.print(" : ");
						AboraSupport.logger.print(cat.name());
						AboraSupport.logger.print("\n"+
"");
						AboraSupport.logger.print("	flock size = ");
						AboraSupport.logger.print((handler.flockSize(i)));
						AboraSupport.logger.print("\n"+
"");
					}
					if ( ! (handler.isForgotten(i))) {
						if ((liveFlockTypes.fetch(cat)) == null) {
							liveFlockTypes.store(cat, cat);
							liveFlockCounts.store(cat, 1);
						}
						else {
							liveFlockCounts.store(cat, (liveFlockCounts.fetch(cat)) + 1);
						}
					}
					else {
						if ((forgottenFlockTypes.fetch(cat)) == null) {
							forgottenFlockTypes.store(cat, cat);
							forgottenFlockCounts.store(cat, 1);
						}
						else {
							forgottenFlockCounts.store(cat, (forgottenFlockCounts.fetch(cat)) + 1);
						}
					}
				}
			}
			handler.destroy();
		}
	}
	AboraSupport.logger.print("\n"+
"tally of live flocks.\n"+
"");
	Stepper stomper = liveFlockTypes.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Category cat1 = (Category) stomper.fetch();
		if (cat1 == null) {
			continue ;
		}
		AboraSupport.logger.print((liveFlockCounts.fetch(cat1)));
		AboraSupport.logger.print("	");
		AboraSupport.logger.print(cat1.name());
		AboraSupport.logger.print("\n"+
"");
	}
	stomper.destroy();
	AboraSupport.logger.print("\n"+
"tally of forgotten flocks.\n"+
"");
	Stepper stomper2 = forgottenFlockTypes.stepper();
	for (; stomper2.hasValue(); stomper2.step()) {
		Category cat2 = (Category) stomper2.fetch();
		if (cat2 == null) {
			continue ;
		}
		AboraSupport.logger.print((forgottenFlockCounts.fetch(cat2)));
		AboraSupport.logger.print("	");
		AboraSupport.logger.print(cat2.name());
		AboraSupport.logger.print("\n"+
"");
	}
	stomper2.destroy();
	info.destroy();
	view.destroy();
	anUrdi.destroy();
/*
udanax-top.st:57875:SnarfStatistics methodsFor: 'running'!
{void} tallyFlockTypes
	| anUrdi {Urdi} view {UrdiView} info {SnarfInfoHandler}
		liveFlockCounts {PrimIndexTable} liveFlockTypes {PrimPtr2PtrTable}
		forgottenFlockCounts {PrimIndexTable} forgottenFlockTypes {PrimPtr2PtrTable} |
	liveFlockCounts _ PrimIndexTable make: 255.
	liveFlockTypes _ PrimPtr2PtrTable make: 255.
	forgottenFlockCounts _ PrimIndexTable make: 255.
	forgottenFlockTypes _ PrimPtr2PtrTable make: 255.
	anUrdi _ Urdi urdi: myFilename with: 2.
	view _ anUrdi makeReadView.
	info _ SnarfInfoHandler make: anUrdi with: view.
	self diskCookbook: view with: info.
	cerr << 'Tallying types over all snarfs, this may take a while.
'.
	info snarfInfoCount almostTo: info snarfCount do: [:snarfID {Int32} |
		(info getSpaceLeft: snarfID) < (view getDataSizeOfSnarf: snarfID)
			ifTrue: 
				[| handler {SnarfHandler} count {Int32} |
				handler _ SnarfHandler make: (view makeReadHandle: snarfID).
				count _ handler mapCount.
				Int32Zero almostTo: count do: [:i {Int32} |
					((handler isOccupied: i) and: [(handler fetchForward: i) == NULL]) ifTrue:
						[| rcvr {Rcvr} stream {XnReadStream} cat {Category} |
						rcvr _ self makeRcvr: (stream _ handler readStream: i).
						cat _ SpecialistRcvrJig receiveCategory: rcvr.
						rcvr destroy.
						stream destroy.
						(cat isEqualOrSubclassOf: Abraham) ifFalse:
							[cerr << 'WARNING: non-Abraham flock at ' << snarfID << ':' << i << ' : '.
							cerr << cat name << '
'.
							cerr << '	flock size = ' << (handler flockSize: i) << '
'].
						(handler isForgotten: i) ifFalse:
							[(liveFlockTypes fetch: cat) == NULL
								ifTrue: [liveFlockTypes at: cat store: cat.
										liveFlockCounts at: cat store: 1]
								ifFalse: [liveFlockCounts at: cat store: (liveFlockCounts fetch: cat) + 1]]
							ifTrue: [(forgottenFlockTypes fetch: cat) == NULL
								ifTrue: [forgottenFlockTypes at: cat store: cat.
										forgottenFlockCounts at: cat store: 1]
								ifFalse: [forgottenFlockCounts at: cat store: (forgottenFlockCounts fetch: cat) + 1]]]].
				handler destroy]].
	cerr << '
tally of live flocks.
'.
	liveFlockTypes stepper forEach: [:cat {Category} |
		cerr << (liveFlockCounts fetch: cat) << '	' << cat name << '
'].
	cerr << '
tally of forgotten flocks.
'.
	forgottenFlockTypes stepper forEach: [:cat {Category} |
		cerr << (forgottenFlockCounts fetch: cat) << '	' << cat name << '
'].
	info destroy.
	view destroy.
	anUrdi destroy!
*/
}
/**
 * Get the cookbook and protocol-stream maker for the disk.
 */
public void diskCookbook(UrdiView view, SnarfInfoHandler info) {
	SnarfHandler handler;
	XnReadStream stream;
	Rcvr rcvr;
	String protocol;
	String cookbook;
	handler = SnarfHandler.make((view.makeReadHandle(info.snarfInfoCount())));
	rcvr = TextyXcvrMaker.makeReader((stream = handler.readStream(0)));
	protocol = rcvr.receiveString();
	cookbook = rcvr.receiveString();
	rcvr.destroy();
	stream.destroy();
	handler.destroy();
	myProtocol = ProtocolBroker.diskProtocol(protocol);
	myCookbook = Cookbook.makeString(cookbook);
	/* Removed protocol.delete(); */
	/* Removed cookbook.delete(); */
/*
udanax-top.st:57936:SnarfStatistics methodsFor: 'private'!
{void} diskCookbook: view {UrdiView} with: info {SnarfInfoHandler}
	"Get the cookbook and protocol-stream maker for the disk."
	| handler {SnarfHandler} stream {XnReadStream} rcvr {Rcvr} protocol {char star} cookbook {char star}|
	handler _ SnarfHandler make: (view makeReadHandle: info snarfInfoCount).
	rcvr _ TextyXcvrMaker makeReader: (stream _ handler readStream: Int32Zero).
	protocol _ rcvr receiveString.
	cookbook _ rcvr receiveString.
	rcvr destroy.
	stream destroy.
	handler destroy.
	myProtocol _ ProtocolBroker diskProtocol: protocol.	
	myCookbook _ Cookbook make.String: cookbook.
	protocol delete.
	cookbook delete.!
*/
}
public SpecialistRcvr makeRcvr(XnReadStream readStream) {
	return myProtocol.makeRcvr((DiskSpecialist.make(myCookbook, null)), readStream);
/*
udanax-top.st:57952:SnarfStatistics methodsFor: 'private'!
{SpecialistRcvr} makeRcvr: readStream {XnReadStream}
	^myProtocol makeRcvr: (DiskSpecialist make: myCookbook with: NULL) with: readStream!
*/
}
public SnarfStatistics(Rcvr receiver) {
	super(receiver);
	myFilename = receiver.receiveString();
/*
udanax-top.st:57957:SnarfStatistics methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myFilename _ receiver receiveString.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendString(myFilename);
/*
udanax-top.st:57961:SnarfStatistics methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendString: myFilename.!
*/
}
public SnarfStatistics() {
/*

Generated during transformation
*/
}
}
