package org.abora.ug2java.util;

import java.beans.PropertyDescriptor;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;

public class ToStringGenerator {

	private final StringBuffer buffer;
	private boolean isEmpty = true;

	public ToStringGenerator(Object object) {
		buffer = new StringBuffer();
		buffer.append(ClassHelper.getShortName(object.getClass()));
		buffer.append('[');
	}

	public ToStringGenerator add(String name, Object value) {
		String valueString = value != null ? value.toString() : "null";
		add(name, valueString);
		return this;
	}

	public ToStringGenerator add(String value) {
		appendSeparator();
		buffer.append(value);
		return this;
	}

	public ToStringGenerator add(String name, long value) {
		add(name, Long.toString(value));
		return this;
	}

	public ToStringGenerator add(String name, String value) {
		appendSeparator();
		buffer.append(name);
		buffer.append('=');
		buffer.append(value);
		return this;
	}

	private void appendSeparator() {
		if (!isEmpty) {
			buffer.append(';');
		} else {
			isEmpty = false;
		}
	}

	public String end() {
		buffer.append(']');
		return buffer.toString();
	}
}