/*
 * Abora-Ash
 * Part of the Abora hypermedia project: http://www.abora.org
 * Copyright 2003 David G Jones
 */

package info.dgjones.abora.ash.engine;

/**
 */
public class Id extends AboraObject {
	private final long value;
	private static long NEXT_VALUE= 1;
	
	public Id() {
		value = NEXT_VALUE++;
	}
	public Id(AboraObject o) {
		this();
		WorksStorage.getInstance().put(this, o);
	}

	public boolean equals(Object o) {
		if (!(o instanceof Id))
			return false;
		Id id = (Id) o;
		return getValue() == id.getValue();
	}
	public int hashCode() {
		return (int)getValue();
	}
	public long getValue() {
		return value;
	}
	public String asString() {
		return "Id("+getValue()+")";
	}
//	nextId
//		| id |
//		id := (self new)
//					value: NextValue;
//					yourself.
//		NextValue := NextValue + 1.
//		^id!
//
//	nextIdFor: anObject
//		| id |
//		id := self nextId.
//		WorksStorage current addId: id with: anObject.
//		^id! !
}
