package org.abora.gold.java.missing;

import org.abora.gold.cxx.otherclass.CopyRecipe;
import org.abora.gold.java.missing.smalltalk.AboraClass;
import org.abora.gold.java.missing.smalltalk.Association;
import org.abora.gold.xcvr.Rcvr;
import org.abora.gold.xcvr.Recipe;


public class ActualCopyRecipe extends CopyRecipe {

	public ActualCopyRecipe(AboraClass cat, Association cuisine) {
		//TODO types on this or just hacked up to fit
		super(cat.getCategory(), cuisine);
	}
	
	protected static Recipe associationToRecipe(Association assoc) {
		return (Recipe)assoc.value();
	}

	public ActualCopyRecipe() {
		super();
		// TODO Auto-generated constructor stub
	}

	public ActualCopyRecipe(Rcvr receiver) {
		super(receiver);
		// TODO Auto-generated constructor stub
	}

}
