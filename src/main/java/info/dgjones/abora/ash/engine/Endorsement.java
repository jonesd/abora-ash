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
package info.dgjones.abora.ash.engine;

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
