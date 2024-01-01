const fs = require('node:fs');

function transpose(matrix) {
    const rows = matrix.length, cols = matrix[0].length;
    const grid = [];
    for (let j = 0; j < cols; j++) {
        grid[j] = [];
    }
    for (let i = 0; i < rows; i++) {
        for (let j = 0; j < cols; j++) {
            grid[cols - j - 1][i] = matrix[i][j];
        }
    }
    return grid;
}

function FindedIterator(arr, elem) {
    return {
        lastN: -1,
        next: function() {
            let findResult = arr.indexOf(elem, this.lastN + 1)
            if (findResult == -1) {
                return {
                    value: [
                        arr.length,
                        arr.slice(this.lastN + 1, arr.length).filter((x) => x == 'O').length   
                    ],
                    done: true
                }
            } else {
                let result = {
                    value: [
                        findResult,
                        arr.slice(this.lastN + 1, findResult).filter((x) => x == 'O').length
                    ],
                    done: false
                }
                this.lastN = findResult
                return result
            }
        }
    };
}

function scorePerSlice(base, count) {
    let result = (base + base - count + 1) / 2 * count
    return result
}

function getColScore(col) {
    let iterOverSharp = FindedIterator(col, '#')
    let n = iterOverSharp.next();
    let score = 0;
    while (!n.done) {
        score += scorePerSlice(n.value[0], n.value[1])
        n = iterOverSharp.next();
    }
    score += scorePerSlice(n.value[0], n.value[1])
    return score
}

function part1(data) {
    let cols = transpose(data.split("\n").map((row) => row.split(''))).map((col) => col.reverse())
    let score = cols.map((col) => getColScore(col)).reduce((acc, v) => acc + v)
    return score
}

function part2(data) {
    return 0;
}

function main() {
    if (process.argv.length !== 3) {
        console.error('usage: app <input_filename>');
        process.exit(1);
    }

    let filename = process.argv.at(2)
    let data;
    try {
        data = fs.readFileSync(filename, 'utf8').toString();
    } catch (err) {
        console.error(err);
        process.exit(1);
    }

    part1_sln = part1(data)
    part2_sln = part2(data)

    console.log(`part1_sln = ${part1_sln}`)
    console.log(`part2_sln = ${part2_sln}`)
}

main()
