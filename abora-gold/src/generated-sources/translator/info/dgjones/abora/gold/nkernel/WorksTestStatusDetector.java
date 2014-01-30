/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.nkernel;

import info.dgjones.abora.gold.be.basic.ID;
import info.dgjones.abora.gold.detect.FeStatusDetector;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeWork;
import info.dgjones.abora.gold.nkernel.WorksTestStatusDetector;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class WorksTestStatusDetector extends FeStatusDetector {

	protected String myTag;
	protected PrintWriter myOutput;
/*
udanax-top.st:19868:
FeStatusDetector subclass: #WorksTestStatusDetector
	instanceVariableNames: '
		myTag {Character star}
		myOutput {ostream star}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-nkernel'!
*/
/*
udanax-top.st:19874:
(WorksTestStatusDetector getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:19907:
WorksTestStatusDetector class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:19910:
(WorksTestStatusDetector getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(WorksTestStatusDetector.class).setAttributes( new Set().add("CONCRETE").add("EQ").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public void grabbed(FeWork work, ID author, int reason) {
	AboraSupport.smalltalkOnly();
	{
		myOutput.print(myTag);
		myOutput.print(" canRevise (");
		myOutput.print(author);
		myOutput.print(")\n"+
"");
	}
	AboraSupport.translateOnly();
	{
		/* (*myOutput) << myTag << \" canRevise (\" << author << \")\\n\"; */
	}
/*
udanax-top.st:19879:WorksTestStatusDetector methodsFor: 'triggering'!
{void} grabbed: work {FeWork} with: author {ID} with: reason {IntegerVar}
	[myOutput << myTag << ' canRevise (' << author << ')
'] smalltalkOnly.
	'(*myOutput) << myTag << " canRevise (" << author << ")\n";' translateOnly.!
*/
}
public void released(FeWork work, int reason) {
	AboraSupport.smalltalkOnly();
	{
		myOutput.print(myTag);
		myOutput.print(" released\n"+
"");
	}
	AboraSupport.translateOnly();
	{
		/* (*myOutput) << myTag << \" released\\n\"; */
	}
/*
udanax-top.st:19885:WorksTestStatusDetector methodsFor: 'triggering'!
{void} released: work {FeWork} with: reason {IntegerVar}
	[myOutput << myTag << ' released
'] smalltalkOnly.
	'(*myOutput) << myTag << " released\n";' translateOnly!
*/
}
public WorksTestStatusDetector(PrintWriter oo, String tag) {
	super();
	AboraSupport.smalltalkOnly();
	{
		myOutput = oo;
	}
	AboraSupport.translateOnly();
	{
		/* myOutput = &oo; */
	}
	myTag = tag;
/*
udanax-top.st:19893:WorksTestStatusDetector methodsFor: 'private: create'!
create: oo {ostream reference} with: tag {Character star}
	super create.
	[myOutput := oo] smalltalkOnly.
	'myOutput = &oo;' translateOnly.
	myTag := tag.!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:19902:WorksTestStatusDetector methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:19904:WorksTestStatusDetector methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public static FeStatusDetector make(PrintWriter oo, String tag) {
	return new WorksTestStatusDetector(oo, tag);
/*
udanax-top.st:19915:WorksTestStatusDetector class methodsFor: 'pseudo constructors'!
{FeStatusDetector} make: oo {ostream reference} with: tag {Character star}
	^self create: oo with: tag.!
*/
}
public WorksTestStatusDetector() {
/*

Generated during transformation
*/
}
public WorksTestStatusDetector(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
