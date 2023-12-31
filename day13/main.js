const fs = require('node:fs');

function chechRowPalindrome(pattern, start, end) {
    let rows = pattern.length
    let cols = end - start + 1
    if (cols % 2 == 1)
        return false
    if (cols < 2)
        return false
    for (let i = start, k = 0; k < cols / 2; ++i, ++k) {
        for (let j = 0; j < rows; ++j) {
            if (pattern.at(j).at(i) != pattern.at(j).at(end - k))
                return false
        }
    }
    return true
}

function transposePattern(matrix) {
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

function getValue(pattern) {
    let cols = pattern.at(0).length
    for (let j = cols - 1; j >= 0; --j) {
        let isPalindrome = chechRowPalindrome(pattern, 0, j)
        if (isPalindrome) {
            return [0, j]
        }
    }
    return [-1, -1]
}

function procPattern(rowPattern) {
    let pattern = rowPattern.split('\n').map((row) => row.split(''))
    let result = 0
    let [i, j] = getValue(pattern)
    if (i == -1) {
        pattern = pattern.map((col) => col.reverse())
        let cols = pattern.at(0).length
        let [i, j] = getValue(pattern)
        if (i != -1)
            result = Math.floor((cols - i + cols - j) / 2)
    } else {
        result = Math.floor((i + j + 1) / 2)
    }

    if (result == 0) {
        pattern = transposePattern(pattern)
        let [k, n] = getValue(pattern)
        if (k == -1) {
            pattern = pattern.map((col) => col.reverse())
            let cols = pattern.at(0).length
            let [k, n] = getValue(pattern)
            if (k != -1)
                result += 100 * Math.floor((cols - k + cols - n) / 2)
        } else {
            result += 100 * (k + n + 1) / 2
        }
    }
    return result
}

function part1(data) {
    let result = data.split("\n\n").map((pattern) => {
        return procPattern(pattern)
    }).reduce((acc, item) => acc + item)
    return result
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
