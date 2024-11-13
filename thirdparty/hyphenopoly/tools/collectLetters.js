/* eslint-disable security/detect-non-literal-fs-filename */
/* eslint-disable no-console */
/* eslint-env node */

"use strict";
const fs = require("fs");
const Characterset = require("characterset");


const dir = process.argv[2];

const charSet = new Set();

const transpiledLetter = "u+0041-005A,u+0061-007A,u+00AA,u+00B5,u+00BA,u+00C0-00D6,u+00D8-00F6,u+00F8-02C1,u+02C6-02D1,u+02E0-02E4,u+02EC,u+02EE,u+0370-0374,u+0376,u+0377,u+037A-037D,u+037F,u+0386,u+0388-038A,u+038C,u+038E-03A1,u+03A3-03F5,u+03F7-0481,u+048A-052F,u+0531-0556,u+0559,u+0560-0588,u+05D0-05EA,u+05EF-05F2,u+0620-064A,u+066E,u+066F,u+0671-06D3,u+06D5,u+06E5,u+06E6,u+06EE,u+06EF,u+06FA-06FC,u+06FF,u+0710,u+0712-072F,u+074D-07A5,u+07B1,u+07CA-07EA,u+07F4,u+07F5,u+07FA,u+0800-0815,u+081A,u+0824,u+0828,u+0840-0858,u+0860-086A,u+08A0-08B4,u+08B6-08C7,u+0904-0939,u+093D,u+0950,u+0958-0961,u+0971-0980,u+0985-098C,u+098F,u+0990,u+0993-09A8,u+09AA-09B0,u+09B2,u+09B6-09B9,u+09BD,u+09CE,u+09DC,u+09DD,u+09DF-09E1,u+09F0,u+09F1,u+09FC,u+0A05-0A0A,u+0A0F,u+0A10,u+0A13-0A28,u+0A2A-0A30,u+0A32,u+0A33,u+0A35,u+0A36,u+0A38,u+0A39,u+0A59-0A5C,u+0A5E,u+0A72-0A74,u+0A85-0A8D,u+0A8F-0A91,u+0A93-0AA8,u+0AAA-0AB0,u+0AB2,u+0AB3,u+0AB5-0AB9,u+0ABD,u+0AD0,u+0AE0,u+0AE1,u+0AF9,u+0B05-0B0C,u+0B0F,u+0B10,u+0B13-0B28,u+0B2A-0B30,u+0B32,u+0B33,u+0B35-0B39,u+0B3D,u+0B5C,u+0B5D,u+0B5F-0B61,u+0B71,u+0B83,u+0B85-0B8A,u+0B8E-0B90,u+0B92-0B95,u+0B99,u+0B9A,u+0B9C,u+0B9E,u+0B9F,u+0BA3,u+0BA4,u+0BA8-0BAA,u+0BAE-0BB9,u+0BD0,u+0C05-0C0C,u+0C0E-0C10,u+0C12-0C28,u+0C2A-0C39,u+0C3D,u+0C58-0C5A,u+0C60,u+0C61,u+0C80,u+0C85-0C8C,u+0C8E-0C90,u+0C92-0CA8,u+0CAA-0CB3,u+0CB5-0CB9,u+0CBD,u+0CDE,u+0CE0,u+0CE1,u+0CF1,u+0CF2,u+0D04-0D0C,u+0D0E-0D10,u+0D12-0D3A,u+0D3D,u+0D4E,u+0D54-0D56,u+0D5F-0D61,u+0D7A-0D7F,u+0D85-0D96,u+0D9A-0DB1,u+0DB3-0DBB,u+0DBD,u+0DC0-0DC6,u+0E01-0E30,u+0E32,u+0E33,u+0E40-0E46,u+0E81,u+0E82,u+0E84,u+0E86-0E8A,u+0E8C-0EA3,u+0EA5,u+0EA7-0EB0,u+0EB2,u+0EB3,u+0EBD,u+0EC0-0EC4,u+0EC6,u+0EDC-0EDF,u+0F00,u+0F40-0F47,u+0F49-0F6C,u+0F88-0F8C,u+1000-102A,u+103F,u+1050-1055,u+105A-105D,u+1061,u+1065,u+1066,u+106E-1070,u+1075-1081,u+108E,u+10A0-10C5,u+10C7,u+10CD,u+10D0-10FA,u+10FC-1248,u+124A-124D,u+1250-1256,u+1258,u+125A-125D,u+1260-1288,u+128A-128D,u+1290-12B0,u+12B2-12B5,u+12B8-12BE,u+12C0,u+12C2-12C5,u+12C8-12D6,u+12D8-1310,u+1312-1315,u+1318-135A,u+1380-138F,u+13A0-13F5,u+13F8-13FD,u+1401-166C,u+166F-167F,u+1681-169A,u+16A0-16EA,u+16F1-16F8,u+1700-170C,u+170E-1711,u+1720-1731,u+1740-1751,u+1760-176C,u+176E-1770,u+1780-17B3,u+17D7,u+17DC,u+1820-1878,u+1880-1884,u+1887-18A8,u+18AA,u+18B0-18F5,u+1900-191E,u+1950-196D,u+1970-1974,u+1980-19AB,u+19B0-19C9,u+1A00-1A16,u+1A20-1A54,u+1AA7,u+1B05-1B33,u+1B45-1B4B,u+1B83-1BA0,u+1BAE,u+1BAF,u+1BBA-1BE5,u+1C00-1C23,u+1C4D-1C4F,u+1C5A-1C7D,u+1C80-1C88,u+1C90-1CBA,u+1CBD-1CBF,u+1CE9-1CEC,u+1CEE-1CF3,u+1CF5,u+1CF6,u+1CFA,u+1D00-1DBF,u+1E00-1F15,u+1F18-1F1D,u+1F20-1F45,u+1F48-1F4D,u+1F50-1F57,u+1F59,u+1F5B,u+1F5D,u+1F5F-1F7D,u+1F80-1FB4,u+1FB6-1FBC,u+1FBE,u+1FC2-1FC4,u+1FC6-1FCC,u+1FD0-1FD3,u+1FD6-1FDB,u+1FE0-1FEC,u+1FF2-1FF4,u+1FF6-1FFC,u+2071,u+207F,u+2090-209C,u+2102,u+2107,u+210A-2113,u+2115,u+2119-211D,u+2124,u+2126,u+2128,u+212A-212D,u+212F-2139,u+213C-213F,u+2145-2149,u+214E,u+2183,u+2184,u+2C00-2C2E,u+2C30-2C5E,u+2C60-2CE4,u+2CEB-2CEE,u+2CF2,u+2CF3,u+2D00-2D25,u+2D27,u+2D2D,u+2D30-2D67,u+2D6F,u+2D80-2D96,u+2DA0-2DA6,u+2DA8-2DAE,u+2DB0-2DB6,u+2DB8-2DBE,u+2DC0-2DC6,u+2DC8-2DCE,u+2DD0-2DD6,u+2DD8-2DDE,u+2E2F,u+3005,u+3006,u+3031-3035,u+303B,u+303C,u+3041-3096,u+309D-309F,u+30A1-30FA,u+30FC-30FF,u+3105-312F,u+3131-318E,u+31A0-31BF,u+31F0-31FF,u+3400-4DBF,u+4E00-9FFC,u+A000-A48C,u+A4D0-A4FD,u+A500-A60C,u+A610-A61F,u+A62A,u+A62B,u+A640-A66E,u+A67F-A69D,u+A6A0-A6E5,u+A717-A71F,u+A722-A788,u+A78B-A7BF,u+A7C2-A7CA,u+A7F5-A801,u+A803-A805,u+A807-A80A,u+A80C-A822,u+A840-A873,u+A882-A8B3,u+A8F2-A8F7,u+A8FB,u+A8FD,u+A8FE,u+A90A-A925,u+A930-A946,u+A960-A97C,u+A984-A9B2,u+A9CF,u+A9E0-A9E4,u+A9E6-A9EF,u+A9FA-A9FE,u+AA00-AA28,u+AA40-AA42,u+AA44-AA4B,u+AA60-AA76,u+AA7A,u+AA7E-AAAF,u+AAB1,u+AAB5,u+AAB6,u+AAB9-AABD,u+AAC0,u+AAC2,u+AADB-AADD,u+AAE0-AAEA,u+AAF2-AAF4,u+AB01-AB06,u+AB09-AB0E,u+AB11-AB16,u+AB20-AB26,u+AB28-AB2E,u+AB30-AB5A,u+AB5C-AB69,u+AB70-ABE2,u+AC00-D7A3,u+D7B0-D7C6,u+D7CB-D7FB,u+F900-FA6D,u+FA70-FAD9,u+FB00-FB06,u+FB13-FB17,u+FB1D,u+FB1F-FB28,u+FB2A-FB36,u+FB38-FB3C,u+FB3E,u+FB40,u+FB41,u+FB43,u+FB44,u+FB46-FBB1,u+FBD3-FD3D,u+FD50-FD8F,u+FD92-FDC7,u+FDF0-FDFB,u+FE70-FE74,u+FE76-FEFC,u+FF21-FF3A,u+FF41-FF5A,u+FF66-FFBE,u+FFC2-FFC7,u+FFCA-FFCF,u+FFD2-FFD7,u+FFDA-FFDC";

const ct = Characterset.parseUnicodeRange(transpiledLetter);

/**
 * Read .chr.txt File
 */
function getChrFile(filename) {
    let chrfile = fs.readFileSync(filename, "utf8");
    chrfile = chrfile.trim();
    return chrfile;
}

/**
 * Collect chars to charSet
 */
function extractChars(chars) {
    const charsA = chars.split("");
    charsA.forEach((char) => {
        if (char !== ",\n") {
            charSet.add(char.toLowerCase());
        }
    });
}

fs.readdir(dir, (err, files) => {
    if (err) {
        return console.log("Unable to scan directory: " + err);
    }
    files.forEach((filename) => {
        if (filename.indexOf(".chr.txt") !== -1) {
            extractChars(getChrFile(dir + filename));
        }
    });
    const cr = new Characterset(Array.from(charSet).join(""));
    const cn = cr.intersect(ct);
    cn.toArray().forEach((cc) => {
        console.log(String.fromCharCode(cc));
    });
    return console.log(cn.toRegExp());
});
