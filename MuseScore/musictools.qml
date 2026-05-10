import QtQuick 2.0
import MuseScore 3.0
import FileIO 3.0

MuseScore {
    menuPath:    "Plugins.MusicTools Counterpoint"
    version:     "1.0"
    description: "No selection → check counterpoint. Selection → solve those beats."

    // Workflow:
    //   1. Run ./watch.sh once in a terminal. Leave it running.
    //   2. Either:
    //        • Hit the hotkey with no selection → checker highlights violations.
    //        • Select notes (single or range), hit the hotkey → solver replaces
    //          the selected notes' pitches with a valid counterpoint.
    //      The hotkey is bound to a single plugin entry; mode is auto-chosen.

    property string scorePath:      "/tmp/musescore_check.json"
    property string responsePath:   "/tmp/musescore_check_violations.json"
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
            applyResponse(data)
            Qt.quit()
        }
    }

    // -----------------------------------------------------------------------
    // Entry point — pick check vs solve based on selection
    // -----------------------------------------------------------------------

    property string lastDebug: ""

    onRun: {
        lastDebug = buildSelectionDebug()
        var selectedKeys = gatherSelectedKeys()
        if (selectedKeys === null) runCheck()
        else                       runSolve(selectedKeys)
    }

    function buildSelectionDebug() {
        var lines = []
        lines.push("Element.NOTE=" + (typeof Element !== "undefined" ? Element.NOTE : "undef"))
        lines.push("Element.CHORD=" + (typeof Element !== "undefined" ? Element.CHORD : "undef"))
        lines.push("Element.REST=" + (typeof Element !== "undefined" ? Element.REST : "undef"))
        var sel = curScore.selection
        lines.push("selection: " + (sel ? "obj" : "null"))
        if (sel) {
            lines.push("elements: " +
                (sel.elements === undefined ? "undefined" :
                 sel.elements === null ? "null" :
                 ("len=" + sel.elements.length)))
            lines.push("isRange: " + sel.isRange)
            lines.push("startSegment: " +
                (sel.startSegment ? ("tick=" + sel.startSegment.tick) : "null"))
            lines.push("endSegment: " +
                (sel.endSegment ? ("tick=" + sel.endSegment.tick) : "null"))
            lines.push("startStaff: " + sel.startStaff +
                       " endStaff: " + sel.endStaff)
            if (sel.elements && sel.elements.length > 0) {
                for (var i = 0; i < Math.min(sel.elements.length, 4); i++) {
                    var el = sel.elements[i]
                    if (!el) { lines.push("el[" + i + "]: null"); continue }
                    var info = "el[" + i + "]: type=" + el.type +
                               " name=" + (el.name || "?")
                    if (el.parent) {
                        var p = el.parent
                        info += " p.type=" + p.type +
                                " p.staffIdx=" + p.staffIdx +
                                " p.voice=" + p.voice
                        if (p.parent) info += " p.p.tick=" + p.parent.tick
                    } else {
                        info += " parent=null"
                    }
                    lines.push(info)
                }
            }
        }
        return lines.join(" | ")
    }

    function applyResponse(data) {
        if (data.mode === "solve") applySolveResult(data.results || [])
        else                       applyCheckResult(data.violations || [])
    }

    function newRequestId() {
        return "r" + Math.floor(Math.random() * 1e9).toString(36)
                   + Date.now().toString(36)
    }

    // -----------------------------------------------------------------------
    // Selection introspection
    // -----------------------------------------------------------------------

    // Returns a map "staff:voice:tick" → true for every chord OR rest in the
    // current selection, or null if nothing is selected.
    function gatherSelectedKeys() {
        var sel = curScore.selection
        if (!sel || !sel.elements || sel.elements.length === 0) return null
        var keys = {}
        var any = false
        for (var i = 0; i < sel.elements.length; i++) {
            var el = sel.elements[i]
            if (!el) continue
            // Resolve to the segment-anchored container (chord for notes,
            // rest as itself, chord as itself).
            var anchor = null
            if      (el.type === Element.NOTE)  anchor = el.parent
            else if (el.type === Element.CHORD) anchor = el
            else if (el.type === Element.REST)  anchor = el
            else continue
            if (!anchor || !anchor.parent) continue
            var staffIdx = anchor.staffIdx
            if (staffIdx === undefined && anchor.staff)
                staffIdx = anchor.staff.idx
            if (staffIdx === undefined) continue
            var voice = (anchor.voice !== undefined) ? anchor.voice : 0
            var tick  = anchor.parent.tick
            keys[staffIdx + ":" + voice + ":" + tick] = true
            any = true
        }
        return any ? keys : null
    }

    // -----------------------------------------------------------------------
    // Check mode (unchanged behavior)
    // -----------------------------------------------------------------------

    function runCheck() {
        requestId = newRequestId()
        var parts = buildCheckJson()
        var doc = { id: requestId, mode: "check", parts: parts, _debug: lastDebug }
        scoreFile.write(JSON.stringify(doc))
        pollTimer.attempts = 0
        pollTimer.start()
    }

    function applyCheckResult(violations) {
        curScore.startCmd()
        clearColors()
        clearStatusTexts()
        colorViolations(violations)
        labelViolations(violations)
        curScore.endCmd()
    }

    function abbreviate(rule) {
        return ruleAbbrev[rule] ? ruleAbbrev[rule] : rule
    }

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

    function colorNote(map, partIdx, voiceIdx, stepIdx) {
        if (partIdx >= map.length) return
        var entry = map[partIdx]
        if (voiceIdx >= entry.order.length) return
        var msVoice = entry.order[voiceIdx]
        var noteList = entry.notes[msVoice]
        if (!noteList || stepIdx >= noteList.length) return
        noteList[stepIdx].color = "#ff0000"
    }

    // parts[partIdx] = [ [pitch|null, ...] (voice 0), ... ]
    function buildCheckJson() {
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
    // Solve mode
    // -----------------------------------------------------------------------

    function runSolve(selectedKeys) {
        var map = buildVoiceMap()
        var cpInfo = findCpVoice(map, selectedKeys)
        if (cpInfo === null) { runCheck(); return }
        requestId = newRequestId()
        var parts = buildSolveJson(map, selectedKeys, cpInfo)
        var doc = { id: requestId, mode: "solve", parts: parts, _debug: lastDebug }
        scoreFile.write(JSON.stringify(doc))
        pollTimer.attempts = 0
        pollTimer.start()
    }

    // First (staff, voice) pair (in score order) with at least one selected
    // slot (chord or rest).
    function findCpVoice(map, selectedKeys) {
        for (var staff = 0; staff < map.length; staff++) {
            var entry = map[staff]
            for (var vi = 0; vi < entry.order.length; vi++) {
                var mv = entry.order[vi]
                var slots = entry.slots[mv]
                for (var i = 0; i < slots.length; i++) {
                    if (selectedKeys[staff + ":" + mv + ":" + slots[i].tick])
                        return { staff: staff, voiceIdx: vi, msVoice: mv }
                }
            }
        }
        return null
    }

    // Per-slot {pitch, free} for every voice. Free rules:
    //   • CP voice rest: always free (rests are illegal in 1st species).
    //   • CP voice chord, in selection: free.
    //   • Anything else: known.
    // Rest pitches are emitted as 0 (Lean ignores pitch when free=true).
    // CF voices are expected to have no rests; if they do, behavior is
    // undefined (the solver will see pitch=0 known notes).
    function buildSolveJson(map, selectedKeys, cpInfo) {
        var parts = []
        for (var staff = 0; staff < map.length; staff++) {
            var entry = map[staff]
            var voices = []
            for (var vi = 0; vi < entry.order.length; vi++) {
                var mv = entry.order[vi]
                var slotList = entry.slots[mv]
                var isCp = (staff === cpInfo.staff && mv === cpInfo.msVoice)
                var notes = []
                for (var i = 0; i < slotList.length; i++) {
                    var slot = slotList[i]
                    var pitch, free
                    if (slot.kind === "rest") {
                        pitch = 0
                        free  = isCp
                    } else {
                        pitch = slot.note.pitch
                        free  = isCp &&
                            selectedKeys[staff + ":" + mv + ":" + slot.tick] === true
                    }
                    notes.push({ pitch: pitch, free: free })
                }
                voices.push(notes)
            }
            parts.push(voices)
        }
        return parts
    }

    function applySolveResult(results) {
        curScore.startCmd()
        clearStatusTexts()
        var map = buildVoiceMap()
        for (var i = 0; i < results.length; i++) {
            var r = results[i]
            if (r.error) {
                labelSolveError(map, r.part, r.voice, r.error)
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
        var slots = entry.slots[msVoice]
        var n = Math.min(pitches.length, slots.length)
        for (var i = 0; i < n; i++) {
            var slot = slots[i]
            var pitch = pitches[i]
            if (slot.kind === "chord") {
                var note = slot.note
                if (note.pitch !== pitch) {
                    var tpc = midiToTpc(pitch)
                    note.pitch = pitch
                    note.tpc1  = tpc
                    note.tpc2  = tpc
                }
            } else {
                replaceRestAtTick(partIdx, msVoice, slot.tick, pitch)
            }
        }
    }

    // Walk the (staff, msVoice) until we land on the rest at the given tick,
    // then call cursor.addNote(pitch). MuseScore replaces the rest with a
    // chord of the rest's duration.
    function replaceRestAtTick(staff, msVoice, tick, pitch) {
        var cursor = curScore.newCursor()
        cursor.rewind(0)
        cursor.staffIdx = staff
        cursor.voice = msVoice
        while (cursor.segment) {
            if (cursor.tick === tick) {
                if (cursor.element && cursor.element.type === Element.REST)
                    cursor.addNote(pitch)
                return
            }
            if (cursor.tick > tick) return
            cursor.next()
        }
    }

    function labelSolveError(map, partIdx, voiceIdx, msg) {
        if (partIdx >= map.length) return
        var entry = map[partIdx]
        if (voiceIdx >= entry.order.length) return
        var msVoice = entry.order[voiceIdx]
        addStatusAtStep(partIdx, msVoice, 0, statusMarker + " " + msg)
    }

    // MIDI pitch → MuseScore tonal pitch class. Naturals first; sharps as
    // defensive fallback (the diatonic-C-major solver currently only emits
    // naturals, so the sharp slots aren't exercised today).
    function midiToTpc(midi) {
        var pc = ((midi % 12) + 12) % 12
        // C  C# D  D# E  F  F# G  G# A  A# B
        var tpcs = [14, 21, 16, 23, 18, 13, 20, 15, 22, 17, 24, 19]
        return tpcs[pc]
    }

    // -----------------------------------------------------------------------
    // Voice discovery — used for both modes
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

    // map[part] = { order: [msVoice...],
    //               notes: { msVoice: [Note...] },          (chord-only)
    //               ticks: { msVoice: [tick...] },          (chord-only)
    //               slots: { msVoice: [Slot...] } }         (chords + rests)
    // Slot = { kind: "chord"|"rest", note: Note|null, tick: int }
    function buildVoiceMap() {
        var map = []
        var numStaves = curScore.nstaves
        for (var staff = 0; staff < numStaves; staff++) {
            var voiceFirstSeen = discoverVoices(staff)
            var voiceNotes = {}
            var voiceTicks = {}
            var voiceSlots = {}
            for (var vi = 0; vi < voiceFirstSeen.length; vi++) {
                voiceNotes[voiceFirstSeen[vi]] = []
                voiceTicks[voiceFirstSeen[vi]] = []
                voiceSlots[voiceFirstSeen[vi]] = []
            }
            for (var vi2 = 0; vi2 < voiceFirstSeen.length; vi2++) {
                var mv = voiceFirstSeen[vi2]
                var cursor = curScore.newCursor()
                cursor.rewind(0)
                cursor.staffIdx = staff
                cursor.voice = mv
                while (cursor.segment) {
                    var el = cursor.element
                    if (el) {
                        if (el.type === Element.CHORD) {
                            voiceNotes[mv].push(el.notes[0])
                            voiceTicks[mv].push(cursor.tick)
                            voiceSlots[mv].push({
                                kind: "chord", note: el.notes[0], tick: cursor.tick
                            })
                        } else if (el.type === Element.REST) {
                            voiceSlots[mv].push({
                                kind: "rest", note: null, tick: cursor.tick
                            })
                        }
                    }
                    cursor.next()
                }
            }
            map.push({ order: voiceFirstSeen, notes: voiceNotes,
                       ticks: voiceTicks, slots: voiceSlots })
        }
        return map
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
}
