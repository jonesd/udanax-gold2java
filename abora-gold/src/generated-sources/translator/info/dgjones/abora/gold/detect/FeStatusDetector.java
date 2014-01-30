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
import info.dgjones.abora.gold.detect.FeStatusDetector;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeWork;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * Is notified of changes in the capability of a Work object.
 */
public class FeStatusDetector extends FeDetector {

/*
udanax-top.st:19759:
FeDetector subclass: #FeStatusDetector
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-detect'!
*/
/*
udanax-top.st:19763:
FeStatusDetector comment:
'Is notified of changes in the capability of a Work object.'!
*/
/*
udanax-top.st:19765:
(FeStatusDetector getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; yourself)!
*/
/*
udanax-top.st:19781:
FeStatusDetector class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:19784:
(FeStatusDetector getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeStatusDetector.class).setAttributes( new Set().add("ONCLIENT").add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Essential. The Work has been grabbed, or regrabbed.
 */
public void grabbed(FeWork work, ID author, int reason) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:19770:FeStatusDetector methodsFor: 'triggering'!
{void CLIENT} grabbed: work {FeWork} with: author {ID} with: reason {IntegerVar}
	"Essential. The Work has been grabbed, or regrabbed."
	
	self subclassResponsibility!
*/
}
/**
 * Essential. The revise capability of the Work has been lost.
 */
public void released(FeWork work, int reason) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:19775:FeStatusDetector methodsFor: 'triggering'!
{void CLIENT} released: work {FeWork} with: reason {IntegerVar}
	"Essential. The revise capability of the Work has been lost."
	
	self subclassResponsibility!
*/
}
/**
 * {Int32 CLIENT INLINE} EDIT.U.PERMISSION.U.CHANGED
 * {Int32 CLIENT INLINE} KEYMASTER.U.CHANGED
 * {Int32 CLIENT INLINE} SIGNATURE.U.AUTHORITY.CHANGED
 * {void NOWAIT CLIENT} grabbed: work {PrWork} with: author {PrID} with: reason {PrInteger}
 * {void NOWAIT CLIENT} released: work {PrWork} with: reason {PrInteger}
 */
public static void infostProtocol() {
/*
udanax-top.st:19789:FeStatusDetector class methodsFor: 'smalltalk: system'!
info.stProtocol
"{Int32 CLIENT INLINE} EDIT.U.PERMISSION.U.CHANGED
{Int32 CLIENT INLINE} KEYMASTER.U.CHANGED
{Int32 CLIENT INLINE} SIGNATURE.U.AUTHORITY.CHANGED
{void NOWAIT CLIENT} grabbed: work {PrWork} with: author {PrID} with: reason {PrInteger}
{void NOWAIT CLIENT} released: work {PrWork} with: reason {PrInteger}
"!
*/
}
/**
 * The reason for the change was a change in the permissions required to edit the Work
 */
public static int EDITUPERMISSIONUCHANGED() {
	return 4;
/*
udanax-top.st:19799:FeStatusDetector class methodsFor: 'constants'!
{Int32 CLIENT INLINE} EDIT.U.PERMISSION.U.CHANGED
	"The reason for the change was a change in the permissions required to edit the Work"
	
	^4!
*/
}
/**
 * The reason for the change was a change in authority of the KeyMaster in the Work
 */
public static int KEYMASTERUCHANGED() {
	return 2;
/*
udanax-top.st:19804:FeStatusDetector class methodsFor: 'constants'!
{Int32 CLIENT INLINE} KEYMASTER.U.CHANGED
	"The reason for the change was a change in authority of the KeyMaster in the Work"
	
	^2!
*/
}
/**
 * The reason for the change was a change in signature authority of the CurrentAuthor
 */
public static int SIGNATUREUAUTHORITYCHANGED() {
	return 1;
/*
udanax-top.st:19809:FeStatusDetector class methodsFor: 'constants'!
{Int32 CLIENT INLINE} SIGNATURE.U.AUTHORITY.CHANGED
	"The reason for the change was a change in signature authority of the CurrentAuthor"
	
	^1!
*/
}
public FeStatusDetector() {
/*

Generated during transformation
*/
}
public FeStatusDetector(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
