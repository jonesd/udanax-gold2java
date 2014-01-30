/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.be.canopy;

import info.dgjones.abora.gold.backrec.ResultRecorder;
import info.dgjones.abora.gold.be.basic.BeEdition;
import info.dgjones.abora.gold.be.canopy.AbstractRecorderFinder;
import info.dgjones.abora.gold.be.canopy.CanopyCrum;
import info.dgjones.abora.gold.be.canopy.PropFinder;
import info.dgjones.abora.gold.be.canopy.SensorPropFinder;
import info.dgjones.abora.gold.be.canopy.prop.Prop;
import info.dgjones.abora.gold.fossil.RecorderFossil;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.TransclusionRecorder;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * The finders used to find recorders in the sensor canopy in response to some change in
 * props of a Stamp.
 */
public class AbstractRecorderFinder extends SensorPropFinder {

/*
udanax-top.st:39881:
SensorPropFinder subclass: #AbstractRecorderFinder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Be-Canopy'!
*/
/*
udanax-top.st:39885:
AbstractRecorderFinder comment:
'The finders used to find recorders in the sensor canopy in response to some change in props of a Stamp.'!
*/
/*
udanax-top.st:39887:
(AbstractRecorderFinder getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(AbstractRecorderFinder.class).setAttributes( new Set().add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
public AbstractRecorderFinder() {
	super();
	/* for generated code */
/*
udanax-top.st:39892:AbstractRecorderFinder methodsFor: 'create'!
create
	super create "for generated code"!
*/
}
public AbstractRecorderFinder(int flags) {
	super(flags);
/*
udanax-top.st:39895:AbstractRecorderFinder methodsFor: 'create'!
create: flags {UInt32}
	super create: flags!
*/
}
public PropFinder findPast(BeEdition stamp) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:39901:AbstractRecorderFinder methodsFor: 'accessing'!
{PropFinder} findPast: stamp {BeEdition}
	
	self subclassResponsibility!
*/
}
/**
 * tell whether a prop matches this filter
 */
public boolean match(Prop prop) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:39905:AbstractRecorderFinder methodsFor: 'accessing'!
{BooleanVar} match: prop {Prop}
	"tell whether a prop matches this filter"
	self subclassResponsibility!
*/
}
/**
 * While doing one step of a southward walk in the O-tree,
 * filtered by the sensor canopy,
 * looking for recorders that represent queries that are newly passed by the change of
 * properties,
 * where the object that changed properties and the change itself are represented by my
 * state,
 * record my object into the recorder if it newly passes the recorder's filtering criteria.
 * See class comments of the various subclasses for details on the purpose of each kindOf
 * AbstractRecorderFinder.
 */
public void checkRecorder(ResultRecorder recorder, RecorderFossil fossil) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:39911:AbstractRecorderFinder methodsFor: 'recording'!
{void} checkRecorder: recorder {ResultRecorder}
	with: fossil {RecorderFossil}
	"While doing one step of a southward walk in the O-tree,
	 filtered by the sensor canopy,
	 looking for recorders that represent queries that are newly passed by the change of properties,
	 where the object that changed properties and the change itself are represented by my state,
	 record my object into the recorder if it newly passes the recorder's filtering criteria.
	 
	 See class comments of the various subclasses for details on the purpose of each kindOf AbstractRecorderFinder."
	
	self subclassResponsibility!
*/
}
/**
 * record the stamp into the recorder if I pass this recorder's filters
 * @deprecated
 */
public void checkStamp(BeEdition stamp, TransclusionRecorder recorder) {
	throw new PasseException();
/*
udanax-top.st:39925:AbstractRecorderFinder methodsFor: 'smalltalk: passe'!
{void} checkStamp: stamp {BeEdition} with: recorder {TransclusionRecorder}
	"record the stamp into the recorder if I pass this recorder's filters"
	
	self passe!
*/
}
/**
 * While doing one step of a southward walk in the O-tree,
 * filtered by the sensor canopy,
 * looking for recorders that represent queries that are newly passed by the change of
 * properties in the Stamp
 * (said change in properties being represented by my state),
 * record the stamp into the recorder if the stamp newly passes the fossil's filtering
 * criteria.
 * See class comments of the various subclasses for details on the purpose of each kindOf
 * AbstractRecorderFinder.
 * @deprecated
 */
public void checkStamp(BeEdition stamp, TransclusionRecorder recorder, RecorderFossil fossil) {
	throw new PasseException();
/*
udanax-top.st:39930:AbstractRecorderFinder methodsFor: 'smalltalk: passe'!
{void} checkStamp: stamp {BeEdition} 
	with: recorder {TransclusionRecorder}
	with: fossil {RecorderFossil}
	"While doing one step of a southward walk in the O-tree,
	 filtered by the sensor canopy,
	 looking for recorders that represent queries that are newly passed by the change of properties in the Stamp
	 (said change in properties being represented by my state),
	 record the stamp into the recorder if the stamp newly passes the fossil's filtering criteria.
	 
	 See class comments of the various subclasses for details on the purpose of each kindOf AbstractRecorderFinder."
	
	self passe!
*/
}
public PropFinder oldPass(CanopyCrum crum) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:39945:AbstractRecorderFinder methodsFor: 'smalltalk: suspended'!
{PropFinder} oldPass: crum {CanopyCrum}
	self subclassResponsibility!
*/
}
public AbstractRecorderFinder(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
