/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.gchooks;

import info.dgjones.abora.gold.collection.basic.Int32Array;
import info.dgjones.abora.gold.collection.basic.WeakPtrArray;
import info.dgjones.abora.gold.gchooks.CloseExecutor;
import info.dgjones.abora.gold.java.AboraSocketSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.wparray.XnExecutor;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * This executor manages objects that need to close file descriptors on finalization.
 */
public class CloseExecutor extends XnExecutor {

	protected static Int32Array FDArray;
	protected static WeakPtrArray FileDescriptorHolders;
/*
udanax-top.st:13732:
XnExecutor subclass: #CloseExecutor
	instanceVariableNames: ''
	classVariableNames: '
		FDArray {Int32Array} 
		FileDescriptorHolders {WeakPtrArray} '
	poolDictionaries: ''
	category: 'Xanadu-gchooks'!
*/
/*
udanax-top.st:13738:
CloseExecutor comment:
'This executor manages objects that need to close file descriptors on finalization.'!
*/
/*
udanax-top.st:13740:
(CloseExecutor getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:13765:
CloseExecutor class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:13768:
(CloseExecutor getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(CloseExecutor.class).setAttributes( new Set().add("CONCRETE").add("EQ").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public CloseExecutor() {
	super();
/*
udanax-top.st:13745:CloseExecutor methodsFor: 'protected: create'!
create
	super create!
*/
}
public void execute(int estateIndex) {
	int fd;
	fd = FDArray.intAt(estateIndex);
	if (fd != -1) {
		AboraSocketSupport.close(fd);
		/* Removed translateOnly */
		FDArray.storeInt(estateIndex, -1);
	}
/*
udanax-top.st:13750:CloseExecutor methodsFor: 'invoking'!
{void} execute: estateIndex {Int32}
	| fd {Int32} |
	fd := FDArray intAt: estateIndex.
	fd ~= -1 ifTrue: [
		[fd close] smalltalkOnly.
		'close((int)fd);' translateOnly.
		FDArray at: estateIndex storeInt: -1]!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:13760:CloseExecutor methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:13762:CloseExecutor methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public static void registerHolder(Heaper holder, int fd) {
	int slot;
	slot = 0;
	if (FDArray == null) {
		XnExecutor exec;
		FDArray = Int32Array.make(32);
		exec = new CloseExecutor();
		FileDescriptorHolders = (WeakPtrArray) WeakPtrArray.make(exec, 32);
	}
	slot = FileDescriptorHolders.indexOf(null);
	/* Removed smalltalkOnly */
	if (slot == -1) {
		/* Removed smalltalkOnly */
		slot = FDArray.count();
		FDArray = (Int32Array) (FDArray.copyGrow(16));
		FileDescriptorHolders = (WeakPtrArray) (FileDescriptorHolders.copyGrow(16));
	}
	FDArray.storeInt(slot, fd);
	FileDescriptorHolders.store(slot, holder);
/*
udanax-top.st:13773:CloseExecutor class methodsFor: 'accessing'!
{void} registerHolder: holder {Heaper} with: fd {Int32}
	| slot {Int32} |
	slot _Int32Zero.
	FDArray == NULL ifTrue: [
		| exec {XnExecutor} |
		FDArray := Int32Array make: 32.
		exec := CloseExecutor create.
		FileDescriptorHolders := WeakPtrArray make: exec with: 32].
	slot := FileDescriptorHolders indexOf: NULL.
	[self halt.] smalltalkOnly.
	slot == -1 ifTrue: [
	[self halt]smalltalkOnly.
		slot := FDArray count.
		FDArray := (FDArray copyGrow: 16) cast: Int32Array.
		FileDescriptorHolders := (FileDescriptorHolders copyGrow: 16) cast: WeakPtrArray].
	FDArray at: slot storeInt: fd.
	FileDescriptorHolders at: slot store: holder.!
*/
}
public static void unregisterHolder(Heaper holder, int fd) {
	int slot;
	slot = FileDescriptorHolders.indexOfEQ(holder);
	while (slot != -1 && (slot < FDArray.count() && ((FDArray.intAt(slot)) != fd))) {
		slot = FileDescriptorHolders.indexOfEQ(holder, slot + 1);
	}
	if (slot == -1 || ((FDArray.intAt(slot)) != fd)) {
		throw new AboraRuntimeException(AboraRuntimeException.SANITY_VIOLATION);
	}
	FileDescriptorHolders.store(slot, null);
	FDArray.storeInt(slot, -1);
/*
udanax-top.st:13791:CloseExecutor class methodsFor: 'accessing'!
{void} unregisterHolder: holder {Heaper} with: fd {Int32}
	| slot {Int32} |
	slot := FileDescriptorHolders indexOfEQ: holder.
	[slot ~= -1 and: [slot < FDArray count and: [(FDArray intAt: slot) ~= fd]]] whileTrue: [
		slot := FileDescriptorHolders indexOfEQ: holder with: slot + 1].
	(slot == -1 or: [(FDArray intAt: slot) ~= fd]) ifTrue: [
		Heaper BLAST: #SanityViolation].
	FileDescriptorHolders at: slot store: NULL.
	FDArray at: slot storeInt: -1.!
*/
}
public static void linkTimeNonInherited() {
	FDArray = null;
	FileDescriptorHolders = null;
/*
udanax-top.st:13803:CloseExecutor class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	FDArray := NULL.
	FileDescriptorHolders := NULL!
*/
}
public CloseExecutor(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
