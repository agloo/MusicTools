import QtQuick 2.9
import QtQuick.Controls 2.0
import QtQuick.Layouts 1.1
import Qt.labs.settings 1.0
import MuseScore 3.0
import FileIO 3.0

MuseScore {
    menuPath:    "Plugins.MusicTools Counterpoint"
    version:     "1.1"
    description: "Counterpoint checker / solver. Pick a species and adjust soft-constraint weights."
    pluginType:  "dialog"
    width:       560
    height:      720

    // -----------------------------------------------------------------------
    // Workflow:
    //   1. Run ./watch.sh once in a terminal. Leave it running.
    //   2. Open this dialog from the Plugins menu (or hotkey).
    //   3. Pick a species, adjust weights.
    //   4. Click "Check" with no selection → highlight violations + show score.
    //      Or select notes and click "Solve selected" → solver fills them in
    //      (solver is first-species only for now).
    //
    // Settings (species + weights per species) persist via Qt.labs.settings.
    // -----------------------------------------------------------------------

    property string scorePath:      "/tmp/musescore_check.json"
    property string responsePath:   "/tmp/musescore_check_violations.json"
    property string requestId:      ""
    property string statusMarker:   "⚠"
    property string scoreMarker:    "♪"

    property var ruleAbbrev: ({
        "vertical-interval":            "bad ivl",
        "start-perfect":                "start≠P",
        "end-perfect":                  "end≠P",
        "direct-into-perfect":          "direct→P",
        "strong-dissonant":             "diss(strong)",
        "weak-dissonant":               "diss(weak)",
        "parallel-perfect-strong":      "‖P (strong)",
        "direct-into-perfect-strong":   "direct→P (strong)",
        "unison-strong":                "unison",
        "downbeat-dissonant":           "diss(down)",
        "offbeat-dissonant":            "diss(off)",
        "parallel-perfect-downbeats":   "‖P (down)"
    })

    // Default weights per species — must match Lean defaults in
    // SecondSpeciesSoftWeighted.Weights, ThirdSpeciesSoftWeighted.Weights, etc.
    property var weightSchemas: ({
        "1": [],
        "2": [
            { key: "chromatic",       label: "Chromatic note",           min: -100, max: 100, def: -39 },
            { key: "imperfect",       label: "Imperfect strong-beat",    min: -100, max: 100, def:  40 },
            { key: "contrary",        label: "Contrary motion",          min: -100, max: 100, def:  50 },
            { key: "repeated",        label: "Repeated note",            min: -100, max: 100, def: -29 },
            { key: "startEnd",        label: "Start/end perfect",        min: -100, max: 100, def:  80 },
            { key: "strongConsonant", label: "Strong-beat consonant",    min: -100, max: 100, def:  40 },
            { key: "weakConsonant",   label: "Weak-beat consonant",      min: -100, max: 100, def:  20 },
            { key: "passingTone",     label: "Passing tone",             min: -100, max: 100, def:  10 },
            { key: "parallelPerfect", label: "Parallel perfect",         min: -100, max: 100, def: -60 },
            { key: "directPerfect",   label: "Direct into perfect",      min: -100, max: 100, def: -40 },
            { key: "midUnison",       label: "Mid-piece unison",         min: -100, max: 100, def: -70 }
        ],
        "3": [
            { key: "downbeatConsonant", label: "Downbeat consonant",     min: -100, max: 100, def:  50 },
            { key: "passingTone",       label: "Passing tone",           min: -100, max: 100, def:  30 },
            { key: "cambiata",          label: "Cambiata",               min: -100, max: 100, def:  40 },
            { key: "contraryMotion",    label: "Contrary motion → P",    min: -100, max: 100, def:  50 },
            { key: "repeatedNote",      label: "Repeated note",          min: -100, max: 100, def: -25 },
            { key: "startEnd",          label: "Start/end perfect",      min: -100, max: 100, def:  80 },
            { key: "downbeatDissonant", label: "Downbeat dissonant",     min: -100, max: 100, def: -50 },
            { key: "offbeatDissonant",  label: "Offbeat dissonant",      min: -100, max: 100, def: -30 },
            { key: "parallelPerfect",   label: "Parallel perfect",       min: -100, max: 100, def: -60 },
            { key: "directPerfect",     label: "Direct into perfect",    min: -100, max: 100, def: -40 }
        ],
        "4": [
            { key: "validSuspension",       label: "Valid suspension (4-3 / 7-6)", min: -100, max: 100, def:  60 },
            { key: "syncopation",           label: "Syncopation tie",              min: -100, max: 100, def:  40 },
            { key: "consonance",            label: "Consonance on 3rd beat",       min: -100, max: 100, def:  30 },
            { key: "startEnd",              label: "Start/end perfect",            min: -100, max: 100, def:  80 },
            { key: "penultimateSuspension", label: "Penultimate 7-6",              min: -100, max: 100, def:  70 },
            { key: "invalid",               label: "Invalid suspension",           min: -100, max: 100, def: -40 }
        ],
        // Fifth species reuses the second/third/fourth weights via nested
        // groups; for compactness the dialog shows the most-impactful subset.
        "5": [
            { key: "secondStrongConsonant", label: "2nd: Strong consonant", min: -100, max: 100, def:  40 },
            { key: "secondPassingTone",     label: "2nd: Passing tone",     min: -100, max: 100, def:  10 },
            { key: "thirdPassingTone",      label: "3rd: Passing tone",     min: -100, max: 100, def:  30 },
            { key: "thirdCambiata",         label: "3rd: Cambiata",         min: -100, max: 100, def:  40 },
            { key: "thirdContraryMotion",   label: "3rd: Contrary→P",       min: -100, max: 100, def:  50 },
            { key: "thirdRepeatedNote",     label: "3rd: Repeated note",    min: -100, max: 100, def: -25 },
            { key: "thirdDirectPerfect",    label: "3rd: Direct→P",         min: -100, max: 100, def: -40 },
            { key: "fourthSyncopation",     label: "4th: Syncopation",      min: -100, max: 100, def:  40 },
            { key: "fourthValidSuspension", label: "4th: Valid suspension", min: -100, max: 100, def:  60 },
            { key: "fourthInvalid",         label: "4th: Invalid susp.",    min: -100, max: 100, def: -40 }
        ]
    })

    // Persistent settings: speciesIdx (0..4 → species 1..5) and weightStore
    // (a JSON-encoded object keyed by "species:key").
    Settings {
        id: prefs
        category: "MusicToolsCounterpoint"
        property int speciesIdx: 0
        property string weightStore: "{}"
    }

    property var weightCache: ({})

    Component.onCompleted: {
        try { weightCache = JSON.parse(prefs.weightStore) || {} }
        catch (e) { weightCache = {} }
        rebuildSliders()
    }

    function currentSpecies() { return speciesCombo.currentIndex + 1 }

    function weightKey(species, k) { return species + ":" + k }

    function getWeight(species, schemaEntry) {
        var k = weightKey(species, schemaEntry.key)
        if (weightCache[k] !== undefined) return weightCache[k]
        return schemaEntry.def
    }

    function setWeight(species, key, value) {
        weightCache[weightKey(species, key)] = value
        prefs.weightStore = JSON.stringify(weightCache)
    }

    // Build the weights JSON object for the current species, in the shape the
    // Lean side expects (per-species fields, or nested for species 5).
    function buildWeightsPayload() {
        var sp = currentSpecies()
        var schema = weightSchemas[sp.toString()]
        if (!schema || schema.length === 0) return {}
        if (sp === 5) {
            var out = { second: {}, third: {}, fourth: {} }
            for (var i = 0; i < schema.length; i++) {
                var e = schema[i]
                var v = getWeight(sp, e)
                // Map flat species-5 keys → nested {second|third|fourth}.<field>
                if (e.key.indexOf("second") === 0)      out.second[lower1(e.key.substring(6))]      = v
                else if (e.key.indexOf("third") === 0)  out.third[lower1(e.key.substring(5))]       = v
                else if (e.key.indexOf("fourth") === 0) out.fourth[lower1(e.key.substring(6))]      = v
            }
            return out
        } else {
            var flat = {}
            for (var j = 0; j < schema.length; j++) {
                var ej = schema[j]
                flat[ej.key] = getWeight(sp, ej)
            }
            return flat
        }
    }

    function lower1(s) {
        return s.length === 0 ? s : s.charAt(0).toLowerCase() + s.substring(1)
    }

    // -----------------------------------------------------------------------
    // UI
    // -----------------------------------------------------------------------

    ColumnLayout {
        anchors.fill: parent
        anchors.margins: 12
        spacing: 10

        RowLayout {
            spacing: 8
            Label { text: "Species:" }
            ComboBox {
                id: speciesCombo
                model: ["1 — note vs. note",
                        "2 — 2:1",
                        "3 — 4:1",
                        "4 — suspensions",
                        "5 — florid"]
                currentIndex: prefs.speciesIdx
                onCurrentIndexChanged: {
                    prefs.speciesIdx = currentIndex
                    rebuildSliders()
                }
                Layout.fillWidth: true
            }
        }

        Label {
            id: scoreLabel
            text: ""
            color: "#205020"
            font.bold: true
            visible: text.length > 0
        }

        GroupBox {
            title: "Soft constraint weights"
            Layout.fillWidth: true
            Layout.fillHeight: true
            visible: currentSpecies() !== 1

            ScrollView {
                anchors.fill: parent
                clip: true
                ColumnLayout {
                    id: slidersColumn
                    width: parent ? parent.width : 480
                    spacing: 6
                }
            }
        }

        Label {
            visible: currentSpecies() === 1
            text: "First species has no soft weights — only strict rules."
            wrapMode: Text.Wrap
            Layout.fillWidth: true
        }

        RowLayout {
            spacing: 8
            Layout.fillWidth: true
            Button {
                text: "Check"
                onClicked: runCheck()
                Layout.fillWidth: true
            }
            Button {
                text: "Solve selected"
                onClicked: {
                    var keys = gatherSelectedKeys()
                    if (keys === null) {
                        scoreLabel.text = scoreMarker + " no selection — select notes to solve"
                    } else if (currentSpecies() !== 1) {
                        scoreLabel.text = scoreMarker + " solver is first-species only (for now)"
                    } else {
                        runSolve(keys)
                    }
                }
                Layout.fillWidth: true
            }
            Button {
                text: "Reset weights"
                onClicked: {
                    var sp = currentSpecies()
                    var schema = weightSchemas[sp.toString()]
                    for (var i = 0; i < schema.length; i++) {
                        delete weightCache[weightKey(sp, schema[i].key)]
                    }
                    prefs.weightStore = JSON.stringify(weightCache)
                    rebuildSliders()
                }
            }
            Button {
                text: "Close"
                onClicked: Qt.quit()
            }
        }
    }

    Component {
        id: weightRowComponent
        RowLayout {
            property var entry
            property int sp
            spacing: 8
            Layout.fillWidth: true
            Label {
                text: entry.label
                Layout.preferredWidth: 200
                elide: Text.ElideRight
            }
            Slider {
                id: slider
                from:    entry.min
                to:      entry.max
                value:   getWeight(sp, entry)
                stepSize: 1
                snapMode: Slider.SnapAlways
                Layout.fillWidth: true
                onMoved: {
                    setWeight(sp, entry.key, Math.round(value))
                    valueLabel.text = Math.round(value).toString()
                }
            }
            Label {
                id: valueLabel
                text: Math.round(slider.value).toString()
                Layout.preferredWidth: 40
                horizontalAlignment: Text.AlignRight
            }
        }
    }

    function rebuildSliders() {
        // Drop existing rows.
        for (var i = slidersColumn.children.length - 1; i >= 0; i--) {
            var c = slidersColumn.children[i]
            if (c && c.destroy) c.destroy()
        }
        var sp = currentSpecies()
        var schema = weightSchemas[sp.toString()]
        if (!schema) return
        for (var j = 0; j < schema.length; j++) {
            weightRowComponent.createObject(slidersColumn,
                { entry: schema[j], sp: sp })
        }
    }

    FileIO { id: scoreFile;    source: "/tmp/musescore_check.json" }
    FileIO { id: responseFile; source: "/tmp/musescore_check_violations.json" }

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
                return
            }
            var raw = responseFile.read()
            if (!raw) return
            var data
            try { data = JSON.parse(raw) } catch (e) { return }
            if (!data || data.id !== requestId) return
            stop()
            applyResponse(data)
        }
    }

    onRun: {
        // Dialog is shown automatically. No score-mutation on open.
    }

    // -----------------------------------------------------------------------

    property string lastDebug: ""

    function applyResponse(data) {
        if (data.mode === "solve") {
            applySolveResult(data.results || [])
            scoreLabel.text = scoreMarker + " solve done"
        } else {
            applyCheckResult(data.violations || [])
            var sc = (data.score !== undefined) ? (" — score " + data.score) : ""
            var n = (data.violations || []).length
            scoreLabel.text = scoreMarker + " " + n + " violation" +
                              (n === 1 ? "" : "s") + sc
        }
    }

    function newRequestId() {
        return "r" + Math.floor(Math.random() * 1e9).toString(36)
                   + Date.now().toString(36)
    }

    // -----------------------------------------------------------------------
    // Selection introspection
    // -----------------------------------------------------------------------

    function gatherSelectedKeys() {
        var sel = curScore.selection
        if (!sel || !sel.elements || sel.elements.length === 0) return null
        var keys = {}
        var any = false
        for (var i = 0; i < sel.elements.length; i++) {
            var el = sel.elements[i]
            if (!el) continue
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
    // Check mode
    // -----------------------------------------------------------------------

    function runCheck() {
        if (!curScore) {
            scoreLabel.text = statusMarker + " no score open"
            return
        }
        requestId = newRequestId()
        var parts = buildCheckJson()
        var doc = {
            id: requestId,
            mode: "check",
            species: currentSpecies(),
            weights: buildWeightsPayload(),
            parts: parts
        }
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
    // Solve mode (first-species only; future work to extend)
    // -----------------------------------------------------------------------

    function runSolve(selectedKeys) {
        var map = buildVoiceMap()
        var cpInfo = findCpVoice(map, selectedKeys)
        if (cpInfo === null) { runCheck(); return }
        requestId = newRequestId()
        var parts = buildSolveJson(map, selectedKeys, cpInfo)
        var doc = { id: requestId, mode: "solve", parts: parts }
        scoreFile.write(JSON.stringify(doc))
        pollTimer.attempts = 0
        pollTimer.start()
    }

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

    function midiToTpc(midi) {
        var pc = ((midi % 12) + 12) % 12
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
