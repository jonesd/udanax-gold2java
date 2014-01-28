/**
 * The MIT License
 * Copyright (c) 2003 David G Jones
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package info.dgjones.abora.gold.java.missing;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.snarf.FlockInfo;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;

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
