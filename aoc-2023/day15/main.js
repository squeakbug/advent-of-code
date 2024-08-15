const fs = require('node:fs');

function part1(data) {
    return 0
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
