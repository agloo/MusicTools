import QtQuick 2.0
import MuseScore 3.0
import FileIO 3.0

MuseScore {
    menuPath:    "Plugins.Check Counterpoint"
    version:     "1.0"
    description: "Highlight first-species counterpoint violations using musescore-check."

    // Workflow:
    //   1. In a terminal, run ./watch.sh once. Leave it running.
    //   2. Edit your score in MuseScore.
    //   3. Hit the plugin hotkey. The plugin writes the score as JSON to
    //      /tmp/musescore_check.json, watch.sh runs the checker, and the
    //      plugin polls /tmp/musescore_check_violations.json for the result.

    property string scorePath:      "/tmp/musescore_check.json"
    property string violationsPath: "/tmp/musescore_check_violations.json"
    property string requestId:      ""
    property string statusMarker:   "⚠"

    property var ruleAbbrev: ({
        "vertical-interval":   "bad ivl",
        "start-perfect":       "start≠P",
        "end-perfect":         "end≠P",
        "direct-into-perfect": "direct→P"
    })

    FileIO {
        id: scoreFile
        source: "/tmp/musescore_check.json"
    }

    FileIO {
        id: violationsFile
        source: "/tmp/musescore_check_violations.json"
    }

    Timer {
        id: pollTimer
        interval: 100
        repeat: true
        property int attempts: 0
        onTriggered: {
            attempts++
            if (attempts > 50) {
                stop()
                curScore.startCmd()
                clearStatusTexts()
                addStatusAtStart("⚠ timeout — is watch.sh running?")
                curScore.endCmd()
                Qt.quit()
                return
            }
            var raw = violationsFile.read()
            if (!raw) return
            var data
            try { data = JSON.parse(raw) } catch (e) { return }
            if (!data || data.id !== requestId) return
            stop()
            applyResult(data.violations || [])
            Qt.quit()
        }
    }

    // -----------------------------------------------------------------------
    // Top-level entry point
    // -----------------------------------------------------------------------

    function runCheck() {
        requestId = newRequestId()
        var parts = buildScoreJson()
        var doc = { id: requestId, parts: parts }
        scoreFile.write(JSON.stringify(doc))
        pollTimer.attempts = 0
        pollTimer.start()
    }

    function applyResult(violations) {
        curScore.startCmd()
        clearColors()
        clearStatusTexts()
        colorViolations(violations)
        labelViolations(violations)
        curScore.endCmd()
    }

    function newRequestId() {
        return "r" + Math.floor(Math.random() * 1e9).toString(36)
                   + Date.now().toString(36)
    }

    function abbreviate(rule) {
        return ruleAbbrev[rule] ? ruleAbbrev[rule] : rule
    }

    // -----------------------------------------------------------------------
    // Status text helpers
    // -----------------------------------------------------------------------

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
    // Color + label
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

    function labelViolations(violations) {
        var map = buildVoiceMap()
        var groups = {}
        for (var i = 0; i < violations.length; i++) {
            var v = violations[i]
            var key = v.part + "/" + v.step
            if (!groups[key]) {
                groups[key] = { part: v.part, step: v.step,
                                voiceA: v.voiceA, rules: [] }
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
    // Voice discovery — used for both the JSON export and the color/label map
    // -----------------------------------------------------------------------

    // Returns the list of MS voice indices that have any chord on `staff`,
    // in first-seen order.
    function discoverVoices(staff) {
        var found = []
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
            if (seen) found.push(msVoice)
        }
        return found
    }

    // map[part] = { order: [msVoice...], notes: { msVoice: [Note...] } }
    function buildVoiceMap() {
        var map = []
        var numStaves = curScore.nstaves
        for (var staff = 0; staff < numStaves; staff++) {
            var voiceFirstSeen = discoverVoices(staff)
            var voiceNotes = {}
            for (var vi = 0; vi < voiceFirstSeen.length; vi++)
                voiceNotes[voiceFirstSeen[vi]] = []
            for (var vi2 = 0; vi2 < voiceFirstSeen.length; vi2++) {
                var mv = voiceFirstSeen[vi2]
                var cursor = curScore.newCursor()
                cursor.rewind(0)
                cursor.staffIdx = staff
                cursor.voice = mv
                while (cursor.segment) {
                    if (cursor.element && cursor.element.type === Element.CHORD)
                        voiceNotes[mv].push(cursor.element.notes[0])
                    cursor.next()
                }
            }
            map.push({ order: voiceFirstSeen, notes: voiceNotes })
        }
        return map
    }

    // parts[partIdx] = [ [pitch|null, ...] (voice 0), ... ]
    function buildScoreJson() {
        var parts = []
        var numStaves = curScore.nstaves
        for (var staff = 0; staff < numStaves; staff++) {
            var order = discoverVoices(staff)
            var voices = []
            for (var i = 0; i < order.length; i++) {
                var mv = order[i]
                var cursor = curScore.newCursor()
                cursor.rewind(0)
                cursor.staffIdx = staff
                cursor.voice = mv
                var pitches = []
                while (cursor.segment) {
                    var el = cursor.element
                    if (el) {
                        if (el.type === Element.CHORD)
                            pitches.push(el.notes[0].pitch)
                        else if (el.type === Element.REST)
                            pitches.push(null)
                    }
                    cursor.next()
                }
                voices.push(pitches)
            }
            parts.push(voices)
        }
        return parts
    }

    // -----------------------------------------------------------------------
    // Plugin lifecycle
    // -----------------------------------------------------------------------

    onRun: {
        runCheck()
        // Qt.quit() is called from the timer once the response arrives or times out.
    }
}
