/*
 * Abora-Ash
 * Part of the Abora hypermedia project: http://www.abora.org
 * Copyright 2003 David G Jones
 */

package org.abora.ash.engine;

/**
 */
public class Endorsement extends AboraObject {
	private final Id endorseeId;
	private final Id itemId;

	public Endorsement(Id endorseeId, Id itemId) {
		this.endorseeId = endorseeId;
		this.itemId = itemId;
	}

	public boolean equals(Object o) {
		if (!(o instanceof Endorsement))
			return false;

		Endorsement e = (Endorsement) o;
		return getEndorseeId().equals(e.getEndorseeId()) && getItemId().equals(e.getItemId());
	}
	public boolean checkIntegrity() {
		return true;
	}
	public AboraObject endorsee() {
		throw new UnsupportedOperationException();

		//		#todo "WorksStorage references only make sense on server. Review!!!!!!".
		//		^WorksStorage current lookupId: endorseeId!
	}
	public Id getEndorseeId() {
		return endorseeId;
	}
	public Id getItemId() {
		return itemId;
	}
	public int hashCode() {
		return (getEndorseeId().hashCode() << 1) | getItemId().hashCode();
	}
	public String asString() {
		return "Endorsement(endorseeId=" + getEndorseeId() + ", itemId=" + getItemId() + ")";

	}
}
