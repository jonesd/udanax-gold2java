/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package org.abora.gold.java.missing;

import org.abora.gold.java.AboraSupport;
import org.abora.gold.java.missing.smalltalk.Set;
import org.abora.gold.snarf.FlockInfo;
import org.abora.gold.xcvr.Rcvr;
import org.abora.gold.xcvr.Xmtr;
import org.abora.gold.xpp.basic.Category;
import org.abora.gold.xpp.basic.Heaper;

public class ShepherdStub extends Heaper {

	private int hashCode;
	private FlockInfo info;
	private Category cat;
	
	public static void initializeClassAttributes() {
		//TODO just made up out of thin air - totally wrong!!!
		AboraSupport.findAboraClass(ShepherdStub.class).setAttributes( new Set().add("CONCRETE").add("PSEUDOCOPY"));
	}

	public ShepherdStub() {
		super();
	}
	
	public ShepherdStub(Rcvr rcvr) {
		super(rcvr);
		//TODO what is going on with all these stubs?
		
		hashCode = rcvr.receiveUInt32();
		cat = (Category)rcvr.receiveHeaper();
		int snarfId = rcvr.receiveUInt32();
		int index = rcvr.receiveUInt32();
		System.out.println("Unknown SheherdStub(Rcvr) next action...");
		//info = (FlockInfo)rcvr.receiveHeaper();
	}
	
	public void sendSelfTo(Xmtr trans) {
		super.sendSelfTo(trans);
		//TODO just copied from DiskSpecialist.sendHeaperTo - what should be happening?
		trans.sendUInt32(hashCode);
		trans.sendHeaper(cat);
		trans.sendUInt32(info.snarfID());
		trans.sendUInt32(info.index());
	}

	public ShepherdStub(int theHash, FlockInfo info, Category theCategory) {
		//System.out.println("Unconnected ShepherdStub created");
		hashCode = theHash;
		this.info = info;
		cat = theCategory;
	}

	public ShepherdStub(int hash, Category newCat) {
		this(hash, null, newCat);
	}
	
	public Category getCategory() {
		//TODO is this right?
		return cat;
	}
	
	public int hash() {
		//TODO is this right
		//TODO should this be hashForEqual method?
		return hashCode;
	}
}
