/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.detect;

import info.dgjones.abora.gold.be.basic.ID;
import info.dgjones.abora.gold.detect.FeDetector;
import info.dgjones.abora.gold.detect.FeRevisionDetector;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeEdition;
import info.dgjones.abora.gold.nkernel.FeWork;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * Client defines subclasses and passes in an instance in order to be notified of revisions
 * to a Work
 */
public class FeRevisionDetector extends FeDetector {

/*
udanax-top.st:19671:
FeDetector subclass: #FeRevisionDetector
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-detect'!
*/
/*
udanax-top.st:19675:
FeRevisionDetector comment:
'Client defines subclasses and passes in an instance in order to be notified of revisions to a Work'!
*/
/*
udanax-top.st:19677:
(FeRevisionDetector getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; yourself)!
*/
/*
udanax-top.st:19692:
FeRevisionDetector class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:19695:
(FeRevisionDetector getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeRevisionDetector.class).setAttributes( new Set().add("ONCLIENT").add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Essential. The Work has been revised. Gives the Work, the current Edition, the author ID
 * who had it grabbed, the sequence number of the revision to the Work, and the clock time on
 * the Server (note that the clock time is only as reliable as the Server's operating system,
 * which is usually not very).
 */
public void revised(FeWork work, FeEdition contents, ID author, int time, int sequence) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:19682:FeRevisionDetector methodsFor: 'triggering'!
{void CLIENT} revised: work {FeWork}
	with: contents {FeEdition}
	with: author {ID}
	with: time {IntegerVar}
	with: sequence {IntegerVar}
	"Essential. The Work has been revised. Gives the Work, the current Edition, the author ID who had it grabbed, the sequence number of the revision to the Work, and the clock time on the Server (note that the clock time is only as reliable as the Server's operating system, which is usually not very)."
	
	self subclassResponsibility!
*/
}
/**
 * {NOWAIT CLIENT} revised: contents {PrEdition} with: author {PrID} with: time {PrInteger}
 * with: sequence {PrInteger}
 */
public static void infostProtocol() {
/*
udanax-top.st:19700:FeRevisionDetector class methodsFor: 'smalltalk: system'!
info.stProtocol
"{NOWAIT CLIENT} revised: contents {PrEdition} with: author {PrID} with: time {PrInteger} with: sequence {PrInteger}
"!
*/
}
public FeRevisionDetector() {
/*

Generated during transformation
*/
}
public FeRevisionDetector(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
