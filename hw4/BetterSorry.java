import java.util.concurrent.locks.ReentrantLock;

class BetterSorry implements State {
    private byte[] value;
    private byte maxval;
	private ReentrantLock[] locks;

    BetterSorry(byte[] v) {
		value = v;
		maxval = 127;
		locks = new ReentrantLock[v.length];
		for(int i=0; i<v.length; i++) {
			locks[i] = new ReentrantLock();
		}
	}

    BetterSorry(byte[] v, byte m) {
		value = v;
		maxval = m;
		locks = new ReentrantLock[v.length];
		for(int i=0; i<v.length; i++) {
			locks[i] = new ReentrantLock();
		}
	}

    public int size() { return value.length; }

    public byte[] current() {
		return value;
	}

    public boolean swap(int i, int j) {
        if (value[i] <= 0 || value[j] >= maxval) {
            return false;
        }
		if(locks[i].tryLock() && locks[j].tryLock()) {
			value[i]--;
			value[j]++;
		}
        return true;
    }
}