/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.negoti8;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.negoti8.ProtocolBroker;
import info.dgjones.abora.gold.negoti8.ProtocolItem;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.XcvrMaker;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class ProtocolBroker extends Heaper {

	protected static ProtocolItem CommProtocols;
	protected static ProtocolItem DiskProtocols;
	protected static XcvrMaker TheCommProtocol;
	protected static XcvrMaker TheDiskProtocol;
/*
udanax-top.st:40920:
Heaper subclass: #ProtocolBroker
	instanceVariableNames: ''
	classVariableNames: '
		CommProtocols {ProtocolItem} 
		DiskProtocols {ProtocolItem} 
		TheCommProtocol {XcvrMaker} 
		TheDiskProtocol {XcvrMaker} '
	poolDictionaries: ''
	category: 'Xanadu-negoti8'!
*/
/*
udanax-top.st:40928:
(ProtocolBroker getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #EQ; yourself)!
*/
/*
udanax-top.st:40938:
ProtocolBroker class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:40941:
(ProtocolBroker getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #EQ; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(ProtocolBroker.class).setAttributes( new Set().add("DEFERRED").add("EQ"));
/*

Generated during transformation: AddMethod
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:40933:ProtocolBroker methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:40935:ProtocolBroker methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public static void linkTimeNonInherited() {
	DiskProtocols = null;
	CommProtocols = null;
	TheCommProtocol = null;
	TheDiskProtocol = null;
/*
udanax-top.st:40946:ProtocolBroker class methodsFor: 'smalltalk: initialization'!
linkTimeNonInherited
	DiskProtocols _ NULL.
	CommProtocols _ NULL.
	TheCommProtocol _ NULL.
	TheDiskProtocol _ NULL!
*/
}
/**
 * return whichever is the best current protocol.
 */
public static XcvrMaker commProtocol() {
	if (TheCommProtocol == null) {
		TheCommProtocol = (XcvrMaker) (CommProtocols.get("binary1"));
	}
	return TheCommProtocol;
/*
udanax-top.st:40954:ProtocolBroker class methodsFor: 'configuration'!
{XcvrMaker} commProtocol
	"return whichever is the best current protocol."
	TheCommProtocol == NULL ifTrue: [TheCommProtocol _ (CommProtocols get: 'binary1') cast: XcvrMaker].
	^TheCommProtocol!
*/
}
public static XcvrMaker commProtocol(String id) {
	return (XcvrMaker) (CommProtocols.get(id));
/*
udanax-top.st:40959:ProtocolBroker class methodsFor: 'configuration'!
{XcvrMaker} commProtocol: id {char star}
	^(CommProtocols get: id) cast: XcvrMaker!
*/
}
/**
 * return whichever is the best current protocol.
 */
public static XcvrMaker diskProtocol() {
	if (TheDiskProtocol == null) {
		TheDiskProtocol = (XcvrMaker) (DiskProtocols.get("binary1"));
	}
	return TheDiskProtocol;
/*
udanax-top.st:40962:ProtocolBroker class methodsFor: 'configuration'!
{XcvrMaker} diskProtocol
	"return whichever is the best current protocol."
	TheDiskProtocol == NULL ifTrue: [TheDiskProtocol _ (DiskProtocols get: 'binary1') cast: XcvrMaker].
	^TheDiskProtocol!
*/
}
public static XcvrMaker diskProtocol(String id) {
	return (XcvrMaker) (DiskProtocols.get(id));
/*
udanax-top.st:40967:ProtocolBroker class methodsFor: 'configuration'!
{XcvrMaker} diskProtocol: id {char star}
	^(DiskProtocols get: id) cast: XcvrMaker!
*/
}
public static void registerXcvrProtocol(XcvrMaker maker) {
	CommProtocols = new ProtocolItem(maker.id(), maker, CommProtocols);
	DiskProtocols = new ProtocolItem(maker.id(), maker, DiskProtocols);
/*
udanax-top.st:40970:ProtocolBroker class methodsFor: 'configuration'!
{void} registerXcvrProtocol: maker {XcvrMaker}
	CommProtocols _ ProtocolItem create: maker id with: maker with: CommProtocols.
	DiskProtocols _ ProtocolItem create: maker id with: maker with: DiskProtocols.!
*/
}
/**
 * Set the protocol.
 */
public static void setCommProtocol(XcvrMaker maker) {
	TheCommProtocol = maker;
/*
udanax-top.st:40974:ProtocolBroker class methodsFor: 'configuration'!
{void} setCommProtocol: maker {XcvrMaker}
	"Set the protocol."
	
	TheCommProtocol _ maker!
*/
}
/**
 * Set the protocol.
 */
public static void setDiskProtocol(XcvrMaker maker) {
	TheDiskProtocol = maker;
/*
udanax-top.st:40979:ProtocolBroker class methodsFor: 'configuration'!
{void} setDiskProtocol: maker {XcvrMaker}
	"Set the protocol."
	
	TheDiskProtocol _ maker!
*/
}
public ProtocolBroker() {
/*

Generated during transformation
*/
}
public ProtocolBroker(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
