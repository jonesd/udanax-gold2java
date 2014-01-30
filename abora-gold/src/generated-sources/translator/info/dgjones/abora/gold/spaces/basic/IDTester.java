/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.spaces.basic;

import info.dgjones.abora.gold.be.basic.BeGrandMap;
import info.dgjones.abora.gold.be.basic.ID;
import info.dgjones.abora.gold.cobbler.Connection;
import info.dgjones.abora.gold.collection.basic.UInt8Array;
import info.dgjones.abora.gold.collection.sets.ImmuSet;
import info.dgjones.abora.gold.collection.sets.SetAccumulator;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.id.IDRegion;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeServer;
import info.dgjones.abora.gold.spaces.basic.IDTester;
import info.dgjones.abora.gold.spaces.integers.RegionTester;
import info.dgjones.abora.gold.spaces.unordered.IDSpace;
import info.dgjones.abora.gold.tumbler.Sequence;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class IDTester extends RegionTester {

	protected Connection myConnection;
/*
udanax-top.st:59918:
RegionTester subclass: #IDTester
	instanceVariableNames: 'myConnection {Connection NOCOPY}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Spaces-Basic'!
*/
/*
udanax-top.st:59922:
(IDTester getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(IDTester.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}));
/*

Generated during transformation: AddMethod
*/
}
public void destruct() {
	myConnection.destroy();
	super.destruct();
/*
udanax-top.st:59927:IDTester methodsFor: 'init'!
{void} destruct
	myConnection destroy.
	super destruct!
*/
}
public ImmuSet initExamples() {
	SetAccumulator iDs;
	SetAccumulator result;
	Sequence backend;
	ImmuSet base;
	IDSpace s;
	myConnection = Connection.make(AboraSupport.findCategory(FeServer.class));
	myConnection.bootHeaper();
	iDs = SetAccumulator.make();
	backend = FeServer.identifier();
	s = IDSpace.make((backend.withLast(3)), 1);
	iDs.step((ID.usingx(s, backend, 3)));
	iDs.step((ID.usingx(s, backend, 30)));
	iDs.step((ID.usingx(s, (backend.withLast(7)), 77)));
	iDs.step((ID.usingx(s, (backend.withLast(7)), 33)));
	iDs.step((ID.usingx(s, (backend.withLast(5)), 99)));
	result = SetAccumulator.make();
	Stepper stomper = ((ImmuSet) iDs.value()).stepper();
	for (; stomper.hasValue(); stomper.step()) {
		ID iD = (ID) stomper.fetch();
		if (iD == null) {
			continue ;
		}
		result.step(iD.asRegion());
		result.step(iD.asRegion().complement());
	}
	stomper.destroy();
	/* result step: (s iDsFromServer: backend).
	result step: (s iDsFromServer: (backend withLast: 7)).
	result step: (s iDsFromServer: (backend withLast: 9)).
	result step: (s iDsFromServer: backend) complement.
	result step: (s iDsFromServer: (backend withLast: 7))complement.
	result step: (s iDsFromServer: (backend withLast: 9))complement. */
	base = (ImmuSet) result.value();
	Stepper stomper2 = base.stepper();
	for (; stomper2.hasValue(); stomper2.step()) {
		IDRegion r = (IDRegion) stomper2.fetch();
		if (r == null) {
			continue ;
		}
		Stepper stomper3 = base.stepper();
		for (; stomper3.hasValue(); stomper3.step()) {
			IDRegion r2 = (IDRegion) stomper3.fetch();
			if (r2 == null) {
				continue ;
			}
			if (r.hashForEqual() < r2.hashForEqual()) {
				result.step((r.unionWith(r2)));
				result.step((r.intersect(r2)));
			}
		}
		stomper3.destroy();
	}
	stomper2.destroy();
	return (ImmuSet) result.value();
/*
udanax-top.st:59932:IDTester methodsFor: 'init'!
{ImmuSet of: XnRegion} initExamples
	| iDs {SetAccumulator} result {SetAccumulator}
	  backend {Sequence} base {ImmuSet}
	  s {IDSpace} |
	myConnection := Connection make: FeServer.
	myConnection bootHeaper.
	iDs := SetAccumulator make.
	backend := FeServer identifier.
	s := IDSpace make: (backend withLast: 3) with: 1.
	iDs step: (ID usingx: s with: backend with: 3).
	iDs step: (ID usingx: s with: backend with: 30).
	iDs step: (ID usingx: s with: (backend withLast: 7) with: 77).
	iDs step: (ID usingx: s with: (backend withLast: 7) with: 33).
	iDs step: (ID usingx: s with: (backend withLast: 5) with: 99).
	result := SetAccumulator make.
	(iDs value cast: ImmuSet) stepper forEach: [ :iD {ID} |
		result step: iD asRegion.
		result step: iD asRegion complement].
	"result step: (s iDsFromServer: backend).
	result step: (s iDsFromServer: (backend withLast: 7)).
	result step: (s iDsFromServer: (backend withLast: 9)).
	result step: (s iDsFromServer: backend) complement.
	result step: (s iDsFromServer: (backend withLast: 7))complement.
	result step: (s iDsFromServer: (backend withLast: 9))complement."
	
	base := result value cast: ImmuSet.
	base stepper forEach: [ :r {IDRegion} |
		base stepper forEach: [ :r2 {IDRegion} |
			r hashForEqual < r2 hashForEqual ifTrue:
				[result step: (r unionWith: r2).
				result step: (r intersect: r2)]]].
	^result value cast: ImmuSet!
*/
}
public void allTestsOn(PrintWriter oo) {
	super.allTestsOn(oo);
	testImportExportOn(oo);
/*
udanax-top.st:59970:IDTester methodsFor: 'testing'!
{void} allTestsOn: oo {ostream reference}
	super allTestsOn: oo.
	self testImportExportOn: oo.!
*/
}
public void shouldBeEqual(PrintWriter oo, Heaper original, Heaper imported, Heaper importedAgain) {
	boolean oi;
	boolean oa;
	boolean ia;
	oi = original.isEqual(imported);
	oa = original.isEqual(importedAgain);
	ia = imported.isEqual(importedAgain);
	if (oi && ( ! oa && ( ! ia))) {
		oo.print("2nd import ");
		oo.print(importedAgain);
		oo.print(" is different from ");
		oo.print(original);
		oo.print("\n"+
"");
	}
	if (oa && ( ! oi && ( ! ia))) {
		oo.print("import ");
		oo.print(imported);
		oo.print(" is different from ");
		oo.print(original);
		oo.print("\n"+
"");
	}
	if (ia && ( ! oa && ( ! oi))) {
		oo.print("original ");
		oo.print(importedAgain);
		oo.print(" is different from ");
		oo.print(original);
		oo.print("\n"+
"");
	}
/*
udanax-top.st:59974:IDTester methodsFor: 'testing'!
{void} shouldBeEqual: oo {ostream reference}
	with: original {Heaper}
	with: imported {Heaper}
	with: importedAgain {Heaper}
	
	| oi {BooleanVar} oa {BooleanVar} ia {BooleanVar} |
	oi := original isEqual: imported.
	oa := original isEqual: importedAgain.
	ia := imported isEqual: importedAgain.
	(oi and: [oa not and: [ia not]]) ifTrue:
		[oo << '2nd import ' << importedAgain << ' is different from ' << original << '
'].
	(oa and: [oi not and: [ia not]]) ifTrue:
		[oo << 'import ' << imported << ' is different from ' << original << '
'].
	(ia and: [oa not and: [oi not]]) ifTrue:
		[oo << 'original ' << importedAgain << ' is different from ' << original << '
'].!
*/
}
public void shouldBeUnEqual(PrintWriter oo, Heaper original, Heaper imported, Heaper importedAgain) {
	if (original.isEqual(imported)) {
		oo.print("original and import are the same: ");
		oo.print(original);
		oo.print("\n"+
"");
	}
	if (original.isEqual(importedAgain)) {
		oo.print("original and 2nd import are the same: ");
		oo.print(original);
		oo.print("\n"+
"");
	}
	if (imported.isEqual(importedAgain)) {
		oo.print("1st and 2nd import are the same: ");
		oo.print(imported);
		oo.print("\n"+
"");
	}
/*
udanax-top.st:59993:IDTester methodsFor: 'testing'!
{void} shouldBeUnEqual: oo {ostream reference}
	with: original {Heaper}
	with: imported {Heaper}
	with: importedAgain {Heaper}
	
	(original isEqual: imported) ifTrue:
		[oo << 'original and import are the same: ' << original << '
'].
	(original isEqual: importedAgain) ifTrue:
		[oo << 'original and 2nd import are the same: ' << original << '
'].
	( imported isEqual: importedAgain) ifTrue:
		[oo << '1st and 2nd import are the same: ' << imported << '
'].!
*/
}
/**
 * Test an ID
 */
public void testIDOn(PrintWriter oo, ID iD) {
	UInt8Array exported;
	UInt8Array exportedAgain;
	ID imported;
	ID importedAgain;
	exported = iD.export();
	exportedAgain = iD.export();
	if ( ! (exported.contentsEqual(exportedAgain))) {
		oo.print("ID ");
		oo.print(iD);
		oo.print(" exported once to ");
		oo.print(exported);
		oo.print(" and then to ");
		oo.print(exportedAgain);
		oo.print("\n"+
"");
	}
	imported = ID.importx(exported);
	importedAgain = ID.importx(exported);
	shouldBeEqual(oo, iD, imported, importedAgain);
	shouldBeEqual(oo, iD.coordinateSpace(), imported.coordinateSpace(), importedAgain.coordinateSpace());
	shouldBeUnEqual(oo, ((IDSpace) iD.coordinateSpace()).newID(), ((IDSpace) imported.coordinateSpace()).newID(), ((IDSpace) importedAgain.coordinateSpace()).newID());
	oo.print("Finished testing ID ");
	oo.print(iD);
	oo.print("\n"+
"\n"+
"");
/*
udanax-top.st:60008:IDTester methodsFor: 'testing'!
{void} testIDOn: oo {ostream reference} with: iD {ID}
	"Test an ID"
	
	| exported {UInt8Array} exportedAgain {UInt8Array} imported {ID} importedAgain {ID} |
	exported := iD export.
	exportedAgain := iD export.
	(exported contentsEqual: exportedAgain) ifFalse:
		[oo << 'ID ' << iD << ' exported once to ' << exported << ' and then to ' << exportedAgain << '
'].
	imported := ID import: exported.
	importedAgain := ID import: exported.
	self shouldBeEqual: oo with: iD with: imported with: importedAgain.
	self shouldBeEqual: oo with: iD coordinateSpace
		with: imported coordinateSpace
		with: importedAgain coordinateSpace.
	self shouldBeUnEqual: oo
		with: (iD coordinateSpace cast: IDSpace) newID
		with: (imported coordinateSpace cast: IDSpace) newID
		with: (importedAgain coordinateSpace cast: IDSpace) newID.
	oo << 'Finished testing ID ' << iD << '
'!
*/
}
/**
 * Test an IDSpace
 */
public void testIDSpaceOn(PrintWriter oo, IDSpace space, IDSpace special) {
	UInt8Array exported;
	UInt8Array exportedAgain;
	IDSpace imported;
	IDSpace importedAgain;
	exported = space.export();
	exportedAgain = space.export();
	if ( ! (exported.contentsEqual(exportedAgain))) {
		oo.print("IDSpace ");
		oo.print(space);
		oo.print(" exported once to ");
		oo.print(exported);
		oo.print(" and then to ");
		oo.print(exportedAgain);
		oo.print("\n"+
"");
	}
	imported = IDSpace.importx(exported);
	importedAgain = IDSpace.importx(exported);
	shouldBeEqual(oo, space, imported, importedAgain);
	shouldBeUnEqual(oo, space.newID(), imported.newID(), importedAgain.newID());
	testIDOn(oo, space.newID());
	testIDOn(oo, (ID.usingx(special, (((BeGrandMap) CurrentGrandMap.fluidGet()).identifier().withLast(33)), 1)));
	oo.print("Finished testing IDSpace ");
	oo.print(space);
	oo.print("\n"+
"");
/*
udanax-top.st:60031:IDTester methodsFor: 'testing'!
{void} testIDSpaceOn: oo {ostream reference} with: space {IDSpace} with: special {IDSpace  | NULL}
	"Test an IDSpace"
	
	| exported {UInt8Array} exportedAgain {UInt8Array} imported {IDSpace} importedAgain {IDSpace} |
	exported := space export.
	exportedAgain := space export.
	(exported contentsEqual: exportedAgain) ifFalse:
		[oo << 'IDSpace ' << space << ' exported once to ' << exported << ' and then to ' << exportedAgain << '
'].
	imported := IDSpace import: exported.
	importedAgain := IDSpace import: exported.
	self shouldBeEqual: oo with: space with: imported with: importedAgain.
	self shouldBeUnEqual: oo
		with: space newID
		with: imported newID
		with: importedAgain newID.
	self testIDOn: oo with: space newID.
	[BeGrandMap] USES.
	self testIDOn: oo with: (ID
		usingx: special with: (CurrentGrandMap fluidGet identifier withLast: 33) with: 1).
	oo << 'Finished testing IDSpace ' << space << '
'!
*/
}
/**
 * Test import/export of ID objects
 */
public void testImportExportOn(PrintWriter oo) {
	IDSpace s;
	s = IDSpace.make((((BeGrandMap) CurrentGrandMap.fluidGet()).identifier().withLast(22)), 7);
	testIDSpaceOn(oo, s, s);
	s = ((BeGrandMap) CurrentGrandMap.fluidGet()).newIDSpace();
	testIDSpaceOn(oo, s, s);
	testIDSpaceOn(oo, ((BeGrandMap) CurrentGrandMap.fluidGet()).globalIDSpace(), null);
/*
udanax-top.st:60054:IDTester methodsFor: 'testing'!
{void} testImportExportOn: oo {ostream reference}
	"Test import/export of ID objects"
	
	| s {IDSpace} |
	s := IDSpace make: (CurrentGrandMap fluidGet identifier withLast: 22) with: 7.
	self testIDSpaceOn: oo with: s with: s.
	s := CurrentGrandMap fluidGet newIDSpace.
	self testIDSpaceOn: oo with: s with: s.
	self testIDSpaceOn: oo with: CurrentGrandMap fluidGet globalIDSpace with: NULL.!
*/
}
public IDTester(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:60066:IDTester methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:60069:IDTester methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public IDTester() {
/*

Generated during transformation
*/
}
}
