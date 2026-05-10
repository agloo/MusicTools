import QtQuick 2.0
import MuseScore 3.0
import FileIO 3.0

MuseScore {
    menuPath:    "Plugins.Solve Counterpoint"
    version:     "1.0"
    description: "Fill colored beats with a valid first-species counterpoint."

    // Workflow:
    //   1. Run ./watch.sh in a terminal (same one the checker uses).
    //   2. In MuseScore, color the notes that should be solved (default blue).
    //      Any pitch is fine for those — it's just a placeholder.
    //   3. Hit the plugin's hotkey. The plugin writes /tmp/musescore_check.json
    //      with mode:"solve", waits for the response, and rewrites the colored
    //      notes' pitches in place (clearing their color back to black).

    property string scorePath:    "/tmp/musescore_check.json"
    property string responsePath: "/tmp/musescore_check_violations.json"
    property string requestId:    ""
    property string statusMarker: "⚠"
    property string freeColor:    "#0000ff"   // notes the user marks as free

    FileIO {
        id: scoreFile
        source: "/tmp/musescore_check.json"
    }

    FileIO {
        id: responseFile
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
                addStatusAtStart(statusMarker + " timeout — is watch.sh running?")
                curScore.endCmd()
                Qt.quit()
                return
            }
            var raw = responseFile.read()
            if (!raw) return
            var data
            try { data = JSON.parse(raw) } catch (e) { return }
            if (!data || data.id !== requestId) return
            stop()
            applySolveResult(data.results || [])
            Qt.quit()
        }
    }

    // -----------------------------------------------------------------------
    // Entry point
    // -----------------------------------------------------------------------

    function runSolve() {
        requestId = newRequestId()
        var parts = buildSolveJson()
        var doc = { id: requestId, mode: "solve", parts: parts }
        scoreFile.write(JSON.stringify(doc))
        pollTimer.attempts = 0
        pollTimer.start()
    }

    function newRequestId() {
        return "r" + Math.floor(Math.random() * 1e9).toString(36)
                   + Date.now().toString(36)
    }

    // -----------------------------------------------------------------------
    // Color helpers
    // -----------------------------------------------------------------------

    function rgbHex(c) {
        if (!c) return ""
        var s = c.toString().toLowerCase()
        if (s.length < 7 || s[0] !== "#") return ""
        return s.substring(1, 7)
    }

    function isFreeColor(c) {
        return rgbHex(c) === rgbHex(freeColor)
    }

    // MIDI pitch → MuseScore tonal pitch class. Naturals first; sharps for
    // accidentals. The current solver only emits diatonic-C-major naturals,
    // so the sharp slots are just defensive fallbacks.
    function midiToTpc(midi) {
        var pc = ((midi % 12) + 12) % 12
        // C  C# D  D# E  F  F# G  G# A  A# B
        var tpcs = [14, 21, 16, 23, 18, 13, 20, 15, 22, 17, 24, 19]
        return tpcs[pc]
    }

    // -----------------------------------------------------------------------
    // Voice discovery (mirrors the checker)
    // -----------------------------------------------------------------------

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

    // parts[partIdx][voiceIdx] = [{pitch, free}, ...]
    function buildSolveJson() {
        var parts = []
        var map = buildVoiceMap()
        for (var staff = 0; staff < map.length; staff++) {
            var entry = map[staff]
            var voices = []
            for (var vi = 0; vi < entry.order.length; vi++) {
                var mv = entry.order[vi]
                var noteList = entry.notes[mv]
                var notes = []
                for (var i = 0; i < noteList.length; i++) {
                    var n = noteList[i]
                    notes.push({ pitch: n.pitch, free: isFreeColor(n.color) })
                }
                voices.push(notes)
            }
            parts.push(voices)
        }
        return parts
    }

    // -----------------------------------------------------------------------
    // Apply solver response
    // -----------------------------------------------------------------------

    function applySolveResult(results) {
        curScore.startCmd()
        clearStatusTexts()
        var map = buildVoiceMap()
        for (var i = 0; i < results.length; i++) {
            var r = results[i]
            if (r.error) {
                labelError(map, r.part, r.voice, r.error)
                continue
            }
            applyPitches(map, r.part, r.voice, r.pitches || [])
        }
        curScore.endCmd()
    }

    function applyPitches(map, partIdx, voiceIdx, pitches) {
        if (partIdx >= map.length) return
        var entry = map[partIdx]
        if (voiceIdx >= entry.order.length) return
        var msVoice = entry.order[voiceIdx]
        var notes = entry.notes[msVoice]
        var n = Math.min(pitches.length, notes.length)
        for (var i = 0; i < n; i++) {
            var note = notes[i]
            var wasFree = isFreeColor(note.color)
            if (note.pitch !== pitches[i]) {
                var tpc = midiToTpc(pitches[i])
                note.pitch = pitches[i]
                note.tpc1  = tpc
                note.tpc2  = tpc
            }
            if (wasFree) note.color = "#000000"
        }
    }

    function labelError(map, partIdx, voiceIdx, msg) {
        if (partIdx >= map.length) return
        var entry = map[partIdx]
        if (voiceIdx >= entry.order.length) return
        var msVoice = entry.order[voiceIdx]
        var notes = entry.notes[msVoice]
        var step = 0
        for (var i = 0; i < notes.length; i++) {
            if (isFreeColor(notes[i].color)) { step = i; break }
        }
        addStatusAtStep(partIdx, msVoice, step, statusMarker + " " + msg)
    }

    // -----------------------------------------------------------------------
    // Status text helpers (shared shape with the checker)
    // -----------------------------------------------------------------------

    function addStatusAtStart(msg) {
        var cursor = curScore.newCursor()
        cursor.rewind(0)
        if (!cursor.segment) return
        var text = newElement(Element.STAFF_TEXT)
        text.text = msg
        cursor.add(text)
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

    onRun: {
        runSolve()
    }
}
