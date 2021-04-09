function isSubset(subset, set) {
    for (const element of subset) {
        if (set.indexOf(element) === -1) return false;
    }
    return true;
}

function getAllSubsets(array, range) {
    return array.reduce((subsets, value) => subsets.concat(subsets.map(set => [...set, value])), [[]])
        .filter(subset => subset.length === range);
}

class SymmetricBIBD {
    constructor(n, k, r2) {
        this.n = n;
        this.k = k;
        this.r2 = r2;
        this.range = [...Array(this.n).keys()].map(x => x + 1);
        this.BIBD = new Set();
        this.pairs = new Map();
        const pairsArray = getAllSubsets(this.range, 2);
        pairsArray.map(pair => (this.pairs.set(JSON.stringify(pair), 0)));
        try {
            if (this.r2 * (this.n - 1) / (this.k - 1) !== this.k) throw 'R2 test not valid';
            this.generateBIBD();
        } catch (e) {
            throw new Error(e);
        }
    }

    generateBIBD() {
        console.log('GENERATE BIBD');
        this.pairs.forEach((value,pair) => {
            const parsedPair = JSON.parse(pair);
            let occurrence = this.pairs.get(pair);
            while (occurrence < this.r2) {
                this.generateBlock(parsedPair);
                occurrence = this.pairs.get(pair);
                if (this.BIBD.size === this.n) return true;
            }
        });
        throw 'Cannot generate such BIBD - generateBIBD';
    }

    generateBlock(pair) {
        console.log(this.BIBD);
        const possibleBlocks = getAllSubsets(this.range, this.k)
            .filter(block => isSubset(pair, block))
            .filter(block => !this.BIBD.has(JSON.stringify(block)));

        console.log(possibleBlocks);
        console.log('GENERATING BLOCK from', pair);
        for (const block of possibleBlocks) {
            console.log('block', block);
            const pairMapCopy = new Map(this.pairs);
            this.BIBD.add(JSON.stringify(block));
            if (this.checkPairsOccurrence(block)) return true;
            else {
                console.log('NO LUCK');
                this.BIBD.delete(JSON.stringify(block));
                this.pairs = pairMapCopy;
            }
        }
        throw 'Cannot generate such BIBD - generateBlock';
    }

    checkPairsOccurrence(block) {
        console.log('CHECK PAIRS OCCURRENCE');
        const blockPairs = getAllSubsets(block, 2);
        for (const pair of blockPairs) {
            const occurrence = this.pairs.get(JSON.stringify(pair));
            console.log('incrementing pair occurrence');
            const incrementedOccurrence = occurrence + 1;
            console.log(pair, incrementedOccurrence);
            if (incrementedOccurrence > this.r2) return false;
            this.pairs.set(JSON.stringify(pair), incrementedOccurrence);
        }
        return true;
    }
}

const BIBD = new SymmetricBIBD(7, 3, 1);
console.log(BIBD.BIBD);
