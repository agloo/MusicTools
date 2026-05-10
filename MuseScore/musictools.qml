import QtQuick 2.0
import MuseScore 3.0
import FileIO 3.0

MuseScore {
    menuPath:    "Plugins.Check Counterpoint"
    version:     "1.0"
    description: "Highlight first-species counterpoint violations using musescore-check."

    // Workflow:
    //   1. File → Export → MusicXML → /tmp/musescore_check.mxl
    //   2. Run:  ./check.sh /tmp/musescore_check.mxl
    //   3. Trigger this plugin to apply colors.
    property string violationsPath: "/tmp/violations.json"

    FileIO {
        id: violationsFile
        source: "/tmp/violations.json"
    }

    // -----------------------------------------------------------------------
    // Top-level entry point
    // -----------------------------------------------------------------------

    property string statusMarker: "⚠"

    property var ruleAbbrev: ({
        "vertical-interval":   "bad ivl",
        "start-perfect":       "start≠P",
        "end-perfect":         "end≠P",
        "direct-into-perfect": "direct→P"
    })

    function abbreviate(rule) {
        return ruleAbbrev[rule] ? ruleAbbrev[rule] : rule
    }

    function runCheck() {
        var raw = violationsFile.read()
        if (!raw) {
            curScore.startCmd()
            clearStatusTexts()
            addStatusAtStart("⚠ no violations.json — run ./check.sh first")
            curScore.endCmd()
            return
        }

        var violations
        try {
            violations = JSON.parse(raw)
        } catch (e) {
            curScore.startCmd()
            clearStatusTexts()
            addStatusAtStart("⚠ bad JSON: " + e)
            curScore.endCmd()
            return
        }

        curScore.startCmd()
        clearColors()
        clearStatusTexts()
        colorViolations(violations)
        labelViolations(violations)
        curScore.endCmd()
    }

    function addStatusAtStart(msg) {
        var cursor = curScore.newCursor()
        cursor.rewind(0)
        if (!cursor.segment) return
        var text = newElement(Element.STAFF_TEXT)
        text.text = msg
        cursor.add(text)
    }

    function clearStatusTexts() {
        var cursor = curScore.newCursor()
        cursor.rewind(0)
        while (cursor.segment) {
            var anns = cursor.segment.annotations
            if (anns) {
                for (var i = anns.length - 1; i >= 0; i--) {
                    var a = anns[i]
                    if (a && a.type === Element.STAFF_TEXT &&
                        a.text && a.text.indexOf(statusMarker) === 0) {
                        removeElement(a)
                    }
                }
            }
            cursor.next()
        }
    }

    // -----------------------------------------------------------------------
    // Color helpers
    // -----------------------------------------------------------------------

    function clearColors() {
        var numStaves = curScore.nstaves
        for (var staff = 0; staff < numStaves; staff++) {
            for (var msVoice = 0; msVoice < 4; msVoice++) {
                var cursor = curScore.newCursor()
                cursor.rewind(0)
                cursor.staffIdx = staff
                cursor.voice = msVoice
                while (cursor.segment) {
                    if (cursor.element && cursor.element.type === Element.CHORD) {
                        var notes = cursor.element.notes
                        for (var k = 0; k < notes.length; k++)
                            notes[k].color = "#000000"
                    }
                    cursor.next()
                }
            }
        }
    }

    function colorViolations(violations) {
        var map = buildVoiceMap()
        for (var i = 0; i < violations.length; i++) {
            var v = violations[i]
            colorNote(map, v.part, v.voiceA, v.step)
            colorNote(map, v.part, v.voiceB, v.step)
        }
    }

    // Group violations by (part, step) so multiple rules at the same beat
    // collapse into one staff text, then attach a marked staff text at the
    // first offending voice's chord.
    function labelViolations(violations) {
        var map = buildVoiceMap()
        var groups = {}
        for (var i = 0; i < violations.length; i++) {
            var v = violations[i]
            var key = v.part + "/" + v.step
            if (!groups[key]) {
                groups[key] = {
                    part:    v.part,
                    step:    v.step,
                    voiceA:  v.voiceA,
                    rules:   []
                }
            }
            if (groups[key].rules.indexOf(v.rule) < 0)
                groups[key].rules.push(v.rule)
        }
        for (var k in groups) {
            var g = groups[k]
            if (g.part >= map.length) continue
            var entry = map[g.part]
            if (g.voiceA >= entry.order.length) continue
            var msVoice = entry.order[g.voiceA]
            var labels = []
            for (var r = 0; r < g.rules.length; r++)
                labels.push(abbreviate(g.rules[r]))
            addStatusAtStep(g.part, msVoice, g.step,
                            statusMarker + labels.join("/"))
        }
    }

    function addStatusAtStep(staff, msVoice, step, msg) {
        var cursor = curScore.newCursor()
        cursor.rewind(0)
        cursor.staffIdx = staff
        cursor.voice = msVoice
        var count = 0
        while (cursor.segment) {
            if (cursor.element && cursor.element.type === Element.CHORD) {
                if (count === step) {
                    var text = newElement(Element.STAFF_TEXT)
                    text.text = msg
                    text.placement = Placement.BELOW
                    cursor.add(text)
                    return
                }
                count++
            }
            cursor.next()
        }
    }

    function colorNote(map, partIdx, voiceIdx, stepIdx) {
        if (partIdx >= map.length) return
        var entry = map[partIdx]
        if (voiceIdx >= entry.order.length) return
        var msVoice = entry.order[voiceIdx]
        var noteList = entry.notes[msVoice]
        if (!noteList || stepIdx >= noteList.length) return
        noteList[stepIdx].color = "#ff0000"
    }

    // -----------------------------------------------------------------------
    // Build voice map: map[part][voiceIdx][step] = Note element
    // -----------------------------------------------------------------------

    function buildVoiceMap() {
        var map = []
        var numStaves = curScore.nstaves

        for (var staff = 0; staff < numStaves; staff++) {
            var voiceFirstSeen = []

            for (var msVoice = 0; msVoice < 4; msVoice++) {
                var cursor = curScore.newCursor()
                cursor.rewind(0)
                cursor.staffIdx = staff
                cursor.voice = msVoice
                var seen = false
                while (cursor.segment && !seen) {
                    if (cursor.element && cursor.element.type === Element.CHORD)
                        seen = true
                    cursor.next()
                }
                if (seen) voiceFirstSeen.push(msVoice)
            }

            var voiceNotes = {}
            for (var vi = 0; vi < voiceFirstSeen.length; vi++)
                voiceNotes[voiceFirstSeen[vi]] = []

            for (var vi2 = 0; vi2 < voiceFirstSeen.length; vi2++) {
                var mv = voiceFirstSeen[vi2]
                var cursor2 = curScore.newCursor()
                cursor2.rewind(0)
                cursor2.staffIdx = staff
                cursor2.voice = mv
                while (cursor2.segment) {
                    if (cursor2.element && cursor2.element.type === Element.CHORD)
                        voiceNotes[mv].push(cursor2.element.notes[0])
                    cursor2.next()
                }
            }

            map.push({ order: voiceFirstSeen, notes: voiceNotes })
        }

        return map
    }

    // -----------------------------------------------------------------------
    // Plugin lifecycle
    // -----------------------------------------------------------------------

    onRun: {
        runCheck()
        Qt.quit()
    }
}
