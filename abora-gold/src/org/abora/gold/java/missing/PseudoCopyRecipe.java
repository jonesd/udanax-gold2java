package org.abora.gold.java.missing;

import org.abora.gold.cxx.otherclass.CopyRecipe;
import org.abora.gold.java.missing.smalltalk.AboraClass;
import org.abora.gold.java.missing.smalltalk.Association;
import org.abora.gold.xcvr.Rcvr;
import org.abora.gold.xcvr.Recipe;


public class PseudoCopyRecipe extends CopyRecipe {

	public PseudoCopyRecipe(AboraClass cat, Association cuisine) {
		super(cat.getCategory(), cuisine);
		// TODO Auto-generated constructor stub
	}

	protected static Recipe associationToRecipe(Association assoc) {
		return (Recipe)assoc.value();
	}


	public PseudoCopyRecipe() {
		super();
		// TODO Auto-generated constructor stub
	}

	public PseudoCopyRecipe(Rcvr receiver) {
		super(receiver);
		// TODO Auto-generated constructor stub
	}

}
