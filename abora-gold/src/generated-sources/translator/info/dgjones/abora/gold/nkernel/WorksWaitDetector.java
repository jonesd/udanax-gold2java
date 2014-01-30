/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.nkernel;

import info.dgjones.abora.gold.detect.FeWaitDetector;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.WorksWaitDetector;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

/**
 * This class keeps a pointer to an ostream rather than a reference since class
 * ios::operator=() is private.
 */
public class WorksWaitDetector extends FeWaitDetector {

	protected String myTag;
	protected PrintWriter myOutput;
/*
udanax-top.st:19990:
FeWaitDetector subclass: #WorksWaitDetector
	instanceVariableNames: '
		myTag {Character star}
		myOutput {ostream star}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-nkernel'!
*/
/*
udanax-top.st:19996:
WorksWaitDetector comment:
'This class keeps a pointer to an ostream rather than a reference since class ios::operator=() is private.'!
*/
/*
udanax-top.st:19998:
(WorksWaitDetector getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:20025:
WorksWaitDetector class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:20028:
(WorksWaitDetector getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(WorksWaitDetector.class).setAttributes( new Set().add("CONCRETE").add("EQ").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public WorksWaitDetector(PrintWriter oo, String tag) {
	super();
	myOutput = oo;
	/* Removed translateOnly */
	myTag = tag;
/*
udanax-top.st:20003:WorksWaitDetector methodsFor: 'creation'!
create: oo {ostream reference} with: tag {Character star}
	super create.
	[myOutput := oo] smalltalkOnly.
	'myOutput = &oo;' translateOnly.
	myTag := tag.!
*/
}
public void done() {
	myOutput.print(myTag);
	myOutput.print("\n"+
"");
	/* Removed translateOnly */
/*
udanax-top.st:20012:WorksWaitDetector methodsFor: 'triggering'!
{NOACK CLIENT} done
	[myOutput << myTag << '
'] smalltalkOnly.
	'*myOutput << myTag << "\n";' translateOnly.!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:20020:WorksWaitDetector methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:20022:WorksWaitDetector methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public static FeWaitDetector make(PrintWriter oo, String tag) {
	return new WorksWaitDetector(oo, tag);
/*
udanax-top.st:20033:WorksWaitDetector class methodsFor: 'creation'!
{FeWaitDetector} make: oo {ostream reference} with: tag {Character star}
	^self create: oo with: tag!
*/
}
public WorksWaitDetector() {
/*

Generated during transformation
*/
}
public WorksWaitDetector(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
