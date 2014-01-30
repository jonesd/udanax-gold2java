/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.backrec;

import info.dgjones.abora.gold.backrec.DirectEditionRecorder;
import info.dgjones.abora.gold.backrec.EditionRecorder;
import info.dgjones.abora.gold.filter.Filter;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.tclude.TrailBlazer;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * Represents the a persistent transcluders or rangeTranscluders query with
 * directContainersOnly flag on
 */
public class DirectEditionRecorder extends EditionRecorder {

/*
udanax-top.st:44637:
EditionRecorder subclass: #DirectEditionRecorder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-backrec'!
*/
/*
udanax-top.st:44641:
DirectEditionRecorder comment:
'Represents the a persistent transcluders or rangeTranscluders query with directContainersOnly flag on'!
*/
/*
udanax-top.st:44643:
(DirectEditionRecorder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(DirectEditionRecorder.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public boolean isDirectOnly() {
	return true;
/*
udanax-top.st:44648:DirectEditionRecorder methodsFor: 'accessing'!
{BooleanVar} isDirectOnly
	
	^true!
*/
}
public DirectEditionRecorder(Filter directFilter, Filter indirectFilter, TrailBlazer trailBlazer) {
	super(directFilter, indirectFilter, trailBlazer);
/*
udanax-top.st:44654:DirectEditionRecorder methodsFor: 'create'!
create: directFilter {Filter}
	with: indirectFilter {Filter}
	with: trailBlazer {TrailBlazer}
	
	super create: directFilter
		with: indirectFilter
		with: trailBlazer!
*/
}
public DirectEditionRecorder() {
/*

Generated during transformation
*/
}
public DirectEditionRecorder(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
