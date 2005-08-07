package org.abora.gold.java.missing;

import java.io.PrintWriter;

import org.abora.gold.testing.Tester;
import org.abora.gold.xcvr.Rcvr;


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
