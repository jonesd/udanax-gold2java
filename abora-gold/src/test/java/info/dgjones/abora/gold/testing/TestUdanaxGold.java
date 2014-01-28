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
package info.dgjones.abora.gold.testing;
import java.io.IOException;

import info.dgjones.abora.gold.collection.grand.GrandHashTableTester;
import info.dgjones.abora.gold.collection.settable.SetTableTester;
import info.dgjones.abora.gold.cross.CrossTester;
import info.dgjones.abora.gold.diskman.DiskTester;
import info.dgjones.abora.gold.java.AboraStartup;
import info.dgjones.abora.gold.java.missing.FHashTester;
import info.dgjones.abora.gold.nkernel.VolumeTester;
import info.dgjones.abora.gold.nkernel.WorksTester;
import info.dgjones.abora.gold.primtab.PrimIndexTableTester;
import info.dgjones.abora.gold.primtab.PrimPtrTableTester;
import info.dgjones.abora.gold.sheph.ShepherdLockTester;
import info.dgjones.abora.gold.spaces.basic.IDTester;
import info.dgjones.abora.gold.spaces.basic.RealTester;
import info.dgjones.abora.gold.spaces.integers.IntegerRegionTester;
import info.dgjones.abora.gold.tabent.TableEntryTester;
import info.dgjones.abora.gold.xcvr.ShuffleTester;
import info.dgjones.abora.gold.xpp.become.BecomeTester;


public class TestUdanaxGold extends UdanaxGoldTestCase {

	public TestUdanaxGold() {
		super();
	}

	public TestUdanaxGold(String arg0) {
		super(arg0);
	}
	
	public void testBecomeTester() throws IOException {
		System.out.println("Skipped testBecomeTester");
//		BecomeTester tester = new BecomeTester();
//		assertTester(tester);
	}

	public void testDiskTester() throws Exception {
		//TODO need a full disk initialazion here (see AboraStartup and DiskIniter),
		// but at the moment all the Urdi/SnarfHandle related code is simply placeholder
		System.out.println("Skipped testDiskTester");
		/*
		String filename = "disktester.abora";
		AboraStartup.getInstance().initializeRealDisk(filename);
		try {		
			AboraStartup.getInstance().useRealDisk(filename);
			DiskTester tester = new DiskTester();
			runTester(tester);
		} finally {
			AboraStartup.getInstance().useFakeDisk();
		}
		*/
	}

	public void testFHashTester() throws IOException {
		//Started trace.txt from: c.zip/udanax/gold/tools/xpp/sun/fhash.out
		FHashTester tester = new FHashTester();
		assertTester(tester);
	}

	public void testGrandHashTableTester() throws IOException {
		GrandHashTableTester tester = new GrandHashTableTester();
		assertTester(tester);
	}

	public void testHashSetTester() throws IOException {
		HashSetTester tester = new HashSetTester();
		assertTester(tester);
	}
	
	public void testHelloTester() throws IOException {
		HelloTester tester = new HelloTester();
		assertTester(tester);
	}
	
	public void testHashTableTester() throws IOException {
		HashTableTester tester = new HashTableTester();
		assertTester(tester);
	}
	
	public void testImmuSetTester() throws IOException {
		ImmuSetTester tester = new ImmuSetTester();
		assertTester(tester);
	}

	public void testIntegerTableTester() throws IOException {
		IntegerTableTester tester = new IntegerTableTester();
		assertTester(tester);
	}

	public void testPrimIndexTableTester() throws IOException {
		PrimIndexTableTester tester = new PrimIndexTableTester();
		assertTester(tester);
	}

	public void testPrimPtrTableTester() throws IOException {
		PrimPtrTableTester tester = new PrimPtrTableTester();
		assertTester(tester);
	}

	public void testRegionCrossTester() throws IOException {
		CrossTester tester = new CrossTester();
		assertTester(tester);
	}
	
	public void testRegionIDTester() throws IOException {
		IDTester tester = new IDTester();
		assertTester(tester);
	}

	public void testRegionIntegerRegionTester() throws IOException {
		IntegerRegionTester tester = new IntegerRegionTester();
		assertTester(tester);
	}

	public void testRegionRealTester() throws IOException {
		RealTester tester = new RealTester();
		assertTester(tester);
	}


	public void testSetTableTester() throws IOException {
		//TODO broken during maven migration?
		System.out.println("Skipped testSetTableTester");
		/*
		SetTableTester tester = new SetTableTester();
		assertTester(tester);
		*/
	}

	public void testShepherdLockTester() {
		//TODO needs StackExaminer - but that is mostly stubbed out for now
		System.out.println("Skipped testShepherdLockTester");
		/*
		ShepherdLockTester tester = new ShepherdLockTester();
		runTester(tester);
		*/
	}

	public void testShuffleTester() throws IOException {
		ShuffleTester tester = new ShuffleTester();
		assertTester(tester);
	}

	public void testTableEntryTester() throws IOException {
		TableEntryTester tester = new TableEntryTester();
		assertTester(tester);
	}

	public void testVolumeTester() {
		//TODO testcode stubbed out - some disk accessing and type changing?
		System.out.println("Skipped testVolumeTester");
		/*
		VolumeTester tester = new VolumeTester();
		runTester(tester);
		*/
	}

	public void testWorksTester() throws Exception {
		//TODO setup Test account to login to?
		System.out.println("Skipped testWorksTester");
		/*
		WorksTester tester = new WorksTester();
		runTester(tester);
		*/
	}

}
