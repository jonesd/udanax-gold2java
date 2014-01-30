/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.cxx.classx.stuff;

import info.dgjones.abora.gold.collection.grand.GrandDataPage;
import info.dgjones.abora.gold.collection.grand.GrandEntry;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.cxx.classx.stuff.GrandDataPageStepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class GrandDataPageStepper extends Stepper {

	protected GrandDataPage page;
	protected int entryIndex;
/*
udanax-top.st:53875:
Stepper subclass: #GrandDataPageStepper
	instanceVariableNames: '
		page {GrandDataPage}
		entryIndex {IntegerVar}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cxx-class-stuff'!
*/
/*
udanax-top.st:53881:
(GrandDataPageStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(GrandDataPageStepper.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public GrandEntry entry() {
	return (GrandEntry) (page.entryAt(entryIndex));
/*
udanax-top.st:53886:GrandDataPageStepper methodsFor: 'operations'!
{GrandEntry} entry
	^ (page entryAt: entryIndex) basicCast: GrandEntry!
*/
}
public Heaper fetch() {
	shouldNotImplement();
	return null;
/*
udanax-top.st:53889:GrandDataPageStepper methodsFor: 'operations'!
{Heaper wimpy} fetch
	self shouldNotImplement.
	^ NULL!
*/
}
public boolean hasValue() {
	return entryIndex < page.entryCount();
/*
udanax-top.st:53893:GrandDataPageStepper methodsFor: 'operations'!
{BooleanVar} hasValue
	^ entryIndex < page entryCount!
*/
}
public void step() {
	entryIndex = entryIndex + 1;
	verifyEntry();
/*
udanax-top.st:53896:GrandDataPageStepper methodsFor: 'operations'!
{void} step
	entryIndex _ entryIndex + 1.
	self verifyEntry!
*/
}
public GrandDataPageStepper(GrandDataPage aPage, int index) {
	super();
	page = aPage;
	entryIndex = index;
	verifyEntry();
/*
udanax-top.st:53902:GrandDataPageStepper methodsFor: 'private: create'!
create: aPage {GrandDataPage} with: index {IntegerVar}
	super create.
	page _ aPage.
	entryIndex _ index.
	self verifyEntry.!
*/
}
public void verifyEntry() {
	while (entryIndex < page.entryCount() && ((page.entryAt(entryIndex)) == null)) {
		entryIndex = entryIndex + 1;
	}
/*
udanax-top.st:53910:GrandDataPageStepper methodsFor: 'private: private'!
{void} verifyEntry
	[entryIndex < page entryCount and: [(page entryAt: entryIndex) == NULL]]
		whileTrue:
			[entryIndex _ entryIndex + 1]!
*/
}
public Stepper copy() {
	return new GrandDataPageStepper(page, entryIndex);
/*
udanax-top.st:53917:GrandDataPageStepper methodsFor: 'create'!
{Stepper} copy
	^ GrandDataPageStepper create: page with: entryIndex!
*/
}
public GrandDataPageStepper(GrandDataPage aPage) {
	super();
	page = aPage;
	entryIndex = 0;
	verifyEntry();
/*
udanax-top.st:53920:GrandDataPageStepper methodsFor: 'create'!
create: aPage {GrandDataPage}
	super create.
	page _ aPage.
	entryIndex _ IntegerVar0.
	self verifyEntry!
*/
}
public GrandDataPageStepper() {
/*

Generated during transformation
*/
}
public GrandDataPageStepper(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
