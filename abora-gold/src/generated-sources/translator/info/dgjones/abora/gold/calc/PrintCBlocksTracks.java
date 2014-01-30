/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.calc;

import info.dgjones.abora.gold.calc.PrintCBlocksTracks;
import info.dgjones.abora.gold.fm.support.Thunk;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.snarf.CBlockTracker;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

public class PrintCBlocksTracks extends Thunk {

/*
udanax-top.st:57414:
Thunk subclass: #PrintCBlocksTracks
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-calc'!
*/
/*
udanax-top.st:57418:
(PrintCBlocksTracks getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(PrintCBlocksTracks.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}).add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public void execute() {
	/* PrintCBlocksTracks create execute */
	CBlockTracker.printTrackersOn(AboraSupport.logger);
	/* Removed smalltalkOnly */
/*
udanax-top.st:57423:PrintCBlocksTracks methodsFor: 'operate'!
{void} execute
	""
	"PrintCBlocksTracks create execute"
	
	CBlockTracker printTrackersOn: cerr.
	[cerr endEntry] smalltalkOnly!
*/
}
public PrintCBlocksTracks(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:57432:PrintCBlocksTracks methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:57435:PrintCBlocksTracks methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public PrintCBlocksTracks() {
/*

Generated during transformation
*/
}
}
