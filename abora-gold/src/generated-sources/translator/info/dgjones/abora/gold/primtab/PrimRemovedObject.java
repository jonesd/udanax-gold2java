/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.primtab;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.primtab.PrimRemovedObject;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * A single instance of this exists as a marker for slots in PrimTables where entries have
 * been removed.
 * This object lives on the GC heap to keep weak arrays happy
 */
public class PrimRemovedObject extends Heaper {

	protected static Heaper TheRemovedObject;
/*
udanax-top.st:33817:
Heaper subclass: #PrimRemovedObject
	instanceVariableNames: ''
	classVariableNames: 'TheRemovedObject {Heaper} '
	poolDictionaries: ''
	category: 'Xanadu-primtab'!
*/
/*
udanax-top.st:33821:
PrimRemovedObject comment:
'A single instance of this exists as a marker for slots in PrimTables where entries have been removed.
This object lives on the GC heap to keep weak arrays happy'!
*/
/*
udanax-top.st:33824:
(PrimRemovedObject getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #EQ; add: #NO.GC; add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:33834:
PrimRemovedObject class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:33837:
(PrimRemovedObject getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #EQ; add: #NO.GC; add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(PrimRemovedObject.class).setAttributes( new Set().add("EQ").add("NOGC").add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:33829:PrimRemovedObject methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:33831:PrimRemovedObject methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public static Heaper make() {
	return TheRemovedObject;
/*
udanax-top.st:33842:PrimRemovedObject class methodsFor: 'accessing'!
{Heaper wimpy INLINE} make
	
	^ TheRemovedObject!
*/
}
public static void initTimeNonInherited() {
	TheRemovedObject = new PrimRemovedObject();
/*
udanax-top.st:33848:PrimRemovedObject class methodsFor: 'smalltalk: init'!
initTimeNonInherited
	TheRemovedObject _ PrimRemovedObject create.!
*/
}
public static void linkTimeNonInherited() {
	TheRemovedObject = null;
/*
udanax-top.st:33851:PrimRemovedObject class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	TheRemovedObject _ NULL!
*/
}
public PrimRemovedObject() {
/*

Generated during transformation
*/
}
public PrimRemovedObject(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
