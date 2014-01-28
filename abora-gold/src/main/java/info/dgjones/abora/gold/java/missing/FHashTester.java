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

import java.io.PrintWriter;

import info.dgjones.abora.gold.testing.Tester;
import info.dgjones.abora.gold.xcvr.Rcvr;


public class FHashTester extends Tester {

	public FHashTester() {
		super();
	}

	public FHashTester(Rcvr receiver) {
		super(receiver);
	}


	private void stringTest(String str, PrintWriter oo) {
		oo.print("'");
		oo.print(str);			
		oo.print("'->");
		oo.print(FHash.fastHash(str));		
		oo.println();
	}

	private void stringTest2(String string, int start, int count, PrintWriter oo) {
		String str = string.substring(start, start + count);
		oo.print("'");
		oo.print(string);		
		oo.print("', ");
		oo.print(start);		
		oo.print(", ");
		oo.print(count);		
		oo.print(" ->");
		//TODO should call (str, count)
		oo.print(FHash.fastHash(str));	
		oo.println();
	}


		     public void allTestsOn(PrintWriter oo) {
	  oo.print("The original data for this test was produced by Smalltalk\n");
	  oo.print("This test verifies that the X++ version produces compatible hashes\n");
	  
	  for (int i = 0; i < 20000; i += 431) {
	    oo.print(i);
	    oo.print("->");
	    oo.print(FHash.fastHash(i));
	    oo.println();
	  }

	  oo.println();

	  stringTest("foo", oo);
	  stringTest("oof", oo);
	  stringTest("bar", oo);
	  stringTest("car", oo);
	  stringTest("cars", oo);
	  stringTest("this is a long string to hash", oo);
	  stringTest("this is a longer string to hash", oo);

	//
//	 New tests for the counted, null-processing version.
	//
//	 First check that it matches
	//

	  oo.println();

	  stringTest2("foo",0,3, oo);
	  stringTest2("oof",0,3, oo);
	  stringTest2("bar",0,3, oo);
	  stringTest2("car",0,3, oo);
	  stringTest2("cars",0,4, oo);
	  stringTest2("this is a long string to hash",0,29, oo);
	  stringTest2("this is a longer string to hash",0,31, oo);

	  oo.println();
	  stringTest2("abc_this is a longer string to hash_xyz",4,31, oo);

	  oo.println();
	  stringTest2("abc/this is a longer string to hash_xyz",4,31, oo);
	  stringTest2("abc_this is a longer string to hash/xyz",4,31, oo);
	  stringTest2("abc_/his is a longer string to hash_xyz",4,31, oo);
	  stringTest2("abc_this is a longer string to has/_xyz",4,31, oo);
	  stringTest2("abc_this is a longer*string to hash_xyz",4,31, oo);

	  //TODO
//	  oo.print("\nThe next three replace the star with a null and repeat the\n");
//	  oo.print("hash_xyz, hash/xyz, and has/_xyz tests.  The printing of the\n");
//	  oo.print("string will stop at the null.\n\n");
//
//	  stringTest2("abc_this is a longer\0string to hash_xyz",4,31, oo);
//	  stringTest2("abc_this is a longer\0string to hash/xyz",4,31, oo);
//	  stringTest2("abc_this is a longer\0string to has/_xyz",4,31, oo);
	}

}
