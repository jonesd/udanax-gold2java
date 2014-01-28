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

import info.dgjones.abora.gold.collection.basic.Int32Array;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 *   This is a Fibonacci style pseudo random number generator.
 * Once initialized, values are produced by adding elements from
 * an array of numbers.  The array has a length N, and two
 * indices P and Q, which are separated by some distance D.
 * This array is filled initially with N numbers produces by a
 * congruential pseudo-random number generator from a given seed.
 * The contents of this array should not all be even.
 * The two indices are initially P==0 and Q==D.  Each value
 * is then
 * 	(seed[P] + seed[Q]) mod UInt32Max.
 * When the next value is needed, seed[P] receives the above
 * value, and P and Q are incremented modulo N.  The effect
 * with a word size S, is that a seed with S*N bits where groups
 * of S bits separated by S*D bits are mixed together with each step.
 * If N and D are mutually prime, all bits will eventually influence all
 * other bits yielding cycles with lengths greatly exceeding the number
 * range.  One pair of N and D, 11 and 5, respectively, seem to work
 * quite well, while only needing a small seed array.  Empirically,
 * this seems to generate uniformly distributed values.  According
 * to my source for this algorithm (Van Nostrand Encyclopedia of
 * Computer Science, "random numbers"), this passes most randomness tests.
 * (Actually, I picked this up from one of the Ann Arbor hackers ages ago,
 * who in turn read about it in an article by Guy Steele.  The encyclopedia
 * also happens to mention it.)
 * Since RandomStepper is used by fastHash() which is used in tofux.cxx,
 * I've chosen to take the non-purist route of not making this class
 * a subclass of Stepper, even though it has the same interface.
 * Besides, you wouldn't want to use one of these in a forEach loop anyway.
 */
public class RandomStepper extends Heaper {
    private int nSeeds;
    private int p;
    private int q;
    private Int32Array seeds;
    private int currVal;
    private int idx;
	
	public RandomStepper(int seed, int numSeeds, int sep) {
		super();
		/* This constructor produces a new sequence */
		
		int gen;
		
		seeds = Int32Array.make(numSeeds);
		nSeeds = numSeeds;
		p = 0;
		q = p + sep;

		/* This loop uses a standard congruential pseudorandom number generator to
		   produce the seed data for the Fibonacci generator.
		   The intermediate value in the expression producing 'gen,' below, is not
		   to exceed 2^32. */
		gen = seed;
		for (int i = 0; i < nSeeds; i += 1) {
			seeds.storeUInt(i, gen);
			gen = gen * 65497 + 379 & Integer.MAX_VALUE;
		}

		/* We need to be sure that there are odd numbers in the seed array
		   or else the array will eventually be all zero. */
		boolean foundOdd = false;
		for (int j = 0; j < nSeeds; j += 1) {
		  foundOdd |= (seeds.uIntAt(p) & 1) == 1;
		}
		if (!foundOdd) {
		  /* Just make sure some are odd */
		  for (int i = 0; i < nSeeds; i += 4) {
		    seeds.storeUInt(i, seeds.uIntAt(i)+1);
		  }
		}

		currVal = seeds.uIntAt(p) + seeds.uIntAt(q) & Integer.MAX_VALUE;
		idx = 0;
	}

	public RandomStepper (
			Int32Array origSeeds, 
			int numSeeds, 
			int initP, 
			int initQ, 
			int index) 
	{
		/* This constructor is used by copy to clone an existing sequence */
		
		seeds = Int32Array.make(numSeeds);
		nSeeds = numSeeds;
		p = initP;
		q = initQ;
		for (int i = 0; i < nSeeds; i += 1) {
			seeds.storeUInt(i, origSeeds.uIntAt(i));
		}
		currVal = seeds.uIntAt(p) + seeds.uIntAt(q) & Integer.MAX_VALUE;
		idx = index;
	}

	public static RandomStepper make(int seed, int nSeeds, int sep) {
		return new RandomStepper(seed, nSeeds, sep);
	}

	public static RandomStepper make(int seed) {
		return make(seed, 11, 5);
	}

	public void destruct() {
		seeds.destroy(); 
		seeds = null /* don't want stale (S/CHK)PTRs */;
		super.destruct();
	}


	/* operations */

	public boolean hasValue() {
		return true;
	}

	public void step() {
		seeds.storeUInt(p, currVal);
		p = (p + 1) % nSeeds;
		q = (q + 1) % nSeeds;
		currVal = seeds.uIntAt(p) + seeds.uIntAt(q) & Integer.MAX_VALUE;
		idx += 1;
	}


	/* create */

	public RandomStepper copy () {
		return new RandomStepper(seeds, nSeeds, p, q, idx);
	}


	/* special */

	/**
	 *  This indicates how many steps the sequence has taken; its utility
	 * is questionable.
	 */
	public int index() {
		
		return idx;
	}

	public int value() {
		return currVal;
	}


}
