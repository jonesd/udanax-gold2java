/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.be.ents;

import info.dgjones.abora.gold.arrange.Arrangement;
import info.dgjones.abora.gold.be.ents.SharedData;
import info.dgjones.abora.gold.collection.basic.PrimArray;
import info.dgjones.abora.gold.collection.basic.PrimDataArray;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraAssertionException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.snarf.Abraham;
import info.dgjones.abora.gold.spaces.basic.Dsp;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.x.PrimSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class SharedData extends Abraham {

	protected Arrangement myArrangement;
	protected PrimArray myData;
/*
udanax-top.st:11059:
Abraham subclass: #SharedData
	instanceVariableNames: '
		myArrangement {Arrangement}
		myData {PrimArray}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Ents'!
*/
/*
udanax-top.st:11065:
(SharedData getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(SharedData.class).setAttributes( new Set().add("LOCKED").add("COPY").add("SHEPHERDPATRIARCH").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public int contentsHash() {
	return super.contentsHash() ^ myData.contentsHash();
/*
udanax-top.st:11070:SharedData methodsFor: 'accessing'!
{UInt32} contentsHash
	^super contentsHash
		bitXor: myData contentsHash!
*/
}
public Heaper fetch(Position key) {
	return myData.fetchValue((myArrangement.indexOf(key)));
/*
udanax-top.st:11075:SharedData methodsFor: 'accessing'!
{Heaper | NULL} fetch: key {Position}
	^myData fetchValue: (myArrangement indexOf: key) DOTasLong!
*/
}
/**
 * Transfer my data into the toArray mapping through my arrangement and his arrangement.
 */
public void fill(XnRegion keys, Arrangement toArrange, PrimArray toArray, Dsp dsp) {
	if ( ! (keys.isEmpty())) {
		toArrange.copyElements(toArray, dsp, myData, myArrangement, (dsp.inverseOfAll(keys)));
	}
/*
udanax-top.st:11078:SharedData methodsFor: 'accessing'!
{void} fill: keys {XnRegion} with: toArrange {Arrangement} with: toArray {PrimArray} with: dsp {Dsp} 
	"Transfer my data into the toArray mapping through my arrangement and his arrangement."
	
	keys isEmpty ifFalse:
		[toArrange
			copyElements: toArray
			with: dsp
			with: myData
			with: myArrangement
			with: (dsp inverseOfAll: keys)]!
*/
}
/**
 * Return the primSpec for my data.
 */
public PrimSpec spec() {
	return myData.spec();
/*
udanax-top.st:11089:SharedData methodsFor: 'accessing'!
{PrimSpec} spec
	"Return the primSpec for my data."
	
	^myData spec!
*/
}
public SharedData(PrimDataArray data, Arrangement arrange) {
	super();
	myData = data;
	myArrangement = arrange;
	if ( ! (myData.count() == myArrangement.region().count())) {
		throw new AboraAssertionException("Invalid arrangement");
	}
	newShepherd();
	remember();
/*
udanax-top.st:11096:SharedData methodsFor: 'creation'!
create: data {PrimDataArray} with: arrange {Arrangement} 
	super create.
	myData _ data.
	myArrangement _ arrange.
	myData count = myArrangement region count DOTasLong assert: 'Invalid arrangement'.
	self newShepherd.
	self remember!
*/
}
public SharedData(Rcvr receiver) {
	super(receiver);
	myArrangement = (Arrangement) receiver.receiveHeaper();
	myData = (PrimArray) receiver.receiveHeaper();
/*
udanax-top.st:11106:SharedData methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myArrangement _ receiver receiveHeaper.
	myData _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myArrangement);
	xmtr.sendHeaper(myData);
/*
udanax-top.st:11111:SharedData methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myArrangement.
	xmtr sendHeaper: myData.!
*/
}
public SharedData() {
/*

Generated during transformation
*/
}
}
