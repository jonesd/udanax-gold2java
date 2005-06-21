package org.abora.ug2java.javatoken;



public class CharacterLiteral extends JavaLiteral {

	private final String charValue;
	
	public CharacterLiteral(String charValue) {
		super(generateValue(charValue));
		this.charValue = charValue;
	}
	
	public String getCharValue() {
		return charValue;
	}
	
	private static final String generateValue(String charValue) {
		return "'" + charValue + "'";
	}

}
