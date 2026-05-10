import QtQuick 2.0
import MuseScore 3.0
import FileIO 3.0

MuseScore {
    menuPath:    "Plugins.MusicTools Counterpoint"
    version:     "1.1"
    description: "Counterpoint checker / solver. Pick a species and adjust soft-constraint weights."
    pluginType:  "dialog"
    width:       980
    height:      760

    // -----------------------------------------------------------------------
    // Workflow:
    //   1. Run ./watch.sh once in a terminal. Leave it running.
    //   2. Open this dialog from the Plugins menu (or hotkey).
    //   3. Pick a species, adjust weights.
    //   4. Click "Check" with no selection → highlight violations + show score.
    //      Or select notes and click "Solve selected" → solver fills them in
    //      (solver supports first and second species for now).
    //
    // The UI uses plain QtQuick items for better compatibility inside
    // MuseScore's plugin host.
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

    property var statusTextFragments: [
        "⚠",
        "bad ivl",
        "start≠P",
        "end≠P",
        "direct→P",
        "diss(",
        "‖P",
        "unison",
        "timeout",
        "no CF voice",
        "CP/CF",
        "no solution"
    ]

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

    property int speciesIdx: 0
    property int weightRefresh: 0
    property int leftColumnWidth: 540

    function currentSpecies() { return speciesIdx + 1 }

    function currentWeight(e) {
        return e.value === undefined ? e.def : e.value
    }

    function clampWeight(v, mn, mx) {
        if (v < mn) return mn
        if (v > mx) return mx
        return v
    }

    function resetDisplayedScore() {
        scoreLabel.text = scoreMarker + " selected " + speciesOptions[speciesIdx].title
        pointDetailsText = pointGuideForSpecies(currentSpecies(), null, [], [])
    }

    function adjustWeightByIndex(rowIndex, delta) {
        var schema = weightSchemas[currentSpecies().toString()]
        if (!schema || rowIndex < 0 || rowIndex >= schema.length) return
        var e = schema[rowIndex]
        e.value = clampWeight(currentWeight(e) + delta, e.min, e.max)
        weightRefresh++
        resetDisplayedScore()
    }

    function resetWeightsForSpecies() {
        var schema = weightSchemas[currentSpecies().toString()]
        if (!schema) return
        for (var i = 0; i < schema.length; i++)
            schema[i].value = schema[i].def
        weightRefresh++
        resetDisplayedScore()
    }

    function weightRowsModel(refresh, selectedSpecies) {
        var schema = weightSchemas[selectedSpecies.toString()]
        var rows = []
        if (!schema) return rows
        for (var i = 0; i < schema.length; i++) {
            var e = schema[i]
            rows.push({
                index: i,
                label: e.label,
                value: currentWeight(e),
                def: e.def
            })
        }
        return rows
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
                var v = currentWeight(e)
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
                flat[ej.key] = currentWeight(ej)
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

    property var speciesOptions: [
        { title: "1 - note vs. note",  detail: "First species: strict 1:1 counterpoint" },
        { title: "2 - 2:1",           detail: "Second species: two half notes against each cantus note" },
        { title: "3 - 4:1",           detail: "Third species: four quarter notes against each cantus note" },
        { title: "4 - suspensions",   detail: "Fourth species: tied suspensions and resolutions" },
        { title: "5 - florid",        detail: "Fifth species: mixed rhythmic species" }
    ]

    property string pointDetailsText: "Run Check to see the current score.\nFirst species uses strict rules only; no soft-weight score is added."

    function selectSpecies(i) {
        speciesIdx = i
        weightRefresh++
        scoreLabel.text = scoreMarker + " selected " + speciesOptions[i].title
        pointDetailsText = pointGuideForSpecies(currentSpecies(), null, [], [])
    }

    function closeDialog() {
        pollTimer.stop()
        quit()
    }

    Rectangle {
        id: panel
        anchors.fill: parent
        color: "#f7f7f7"
        border.color: "#b8b8b8"
        border.width: 1

        Text {
            id: titleText
            x: 18
            y: 16
            width: parent.width - 36
            text: "MusicTools Counterpoint"
            color: "#202020"
            font.pixelSize: 22
            font.bold: true
        }

        Text {
            id: helpText
            x: 18
            y: 48
            width: parent.width - 36
            text: "Choose a species, then run Check. Leave watch.sh running in a terminal."
            color: "#555555"
            font.pixelSize: 13
            wrapMode: Text.WordWrap
        }

        Column {
            id: speciesColumn
            x: 18
            y: 92
            width: leftColumnWidth
            spacing: 8

            Repeater {
                model: speciesOptions
                delegate: Rectangle {
                    width: speciesColumn.width
                    height: 58
                    radius: 4
                    color: speciesIdx === index ? "#dfeeff" : "#ffffff"
                    border.color: speciesIdx === index ? "#3b78c2" : "#c8c8c8"
                    border.width: speciesIdx === index ? 2 : 1

                    Text {
                        x: 12
                        y: 8
                        width: parent.width - 24
                        text: modelData.title
                        color: "#202020"
                        font.pixelSize: 16
                        font.bold: true
                        elide: Text.ElideRight
                    }

                    Text {
                        x: 12
                        y: 31
                        width: parent.width - 24
                        text: modelData.detail
                        color: "#555555"
                        font.pixelSize: 12
                        elide: Text.ElideRight
                    }

                    MouseArea {
                        anchors.fill: parent
                        onClicked: selectSpecies(index)
                    }
                }
            }
        }

        Text {
            id: scoreLabel
            x: 18
            y: 424
            width: leftColumnWidth
            text: scoreMarker + " selected " + speciesOptions[speciesIdx].title
            color: "#205020"
            font.pixelSize: 13
            font.bold: true
            wrapMode: Text.WordWrap
        }

        Text {
            x: 18
            y: 448
            width: leftColumnWidth
            text: (currentSpecies() === 1
                ? "First species has no soft weights. Species 2-5 use default soft weights."
                : "Species " + currentSpecies() + " will use the selected soft-constraint weights.")
            color: "#606060"
            font.pixelSize: 12
            wrapMode: Text.WordWrap
        }

        Rectangle {
            id: pointPanel
            x: 18
            y: 480
            width: leftColumnWidth
            height: parent.height - y - 92
            radius: 4
            color: "#ffffff"
            border.color: "#c8c8c8"
            border.width: 1

            Text {
                id: pointTitle
                x: 10
                y: 8
                width: parent.width - 20
                text: "Point system"
                color: "#202020"
                font.pixelSize: 13
                font.bold: true
            }

            Flickable {
                id: pointFlick
                x: 10
                y: 30
                width: parent.width - 20
                height: parent.height - 38
                clip: true
                contentWidth: width
                contentHeight: pointDetails.paintedHeight

                Text {
                    id: pointDetails
                    width: pointFlick.width
                    text: pointDetailsText
                    color: "#555555"
                    font.pixelSize: 11
                    wrapMode: Text.WordWrap
                }
            }
        }

        Rectangle {
            id: weightsPanel
            x: speciesColumn.x + speciesColumn.width + 18
            y: 92
            width: parent.width - x - 18
            height: parent.height - y - 92
            radius: 4
            color: "#ffffff"
            border.color: "#c8c8c8"
            border.width: 1

            Text {
                x: 10
                y: 8
                width: parent.width - 120
                text: "Soft weights"
                color: "#202020"
                font.pixelSize: 13
                font.bold: true
            }

            Rectangle {
                id: resetWeightButton
                x: parent.width - width - 10
                y: 7
                width: 82
                height: 24
                radius: 4
                color: "#ffffff"
                border.color: "#8c8c8c"

                Text {
                    anchors.centerIn: parent
                    text: "Reset"
                    color: "#202020"
                    font.pixelSize: 11
                }

                MouseArea {
                    anchors.fill: parent
                    onClicked: resetWeightsForSpecies()
                }
            }

            Text {
                id: noWeightsText
                x: 10
                y: 44
                width: parent.width - 20
                visible: currentSpecies() === 1
                text: "No soft weights for first species."
                color: "#606060"
                font.pixelSize: 12
                wrapMode: Text.WordWrap
            }

            Flickable {
                id: weightFlick
                x: 10
                y: 40
                width: parent.width - 20
                height: parent.height - 48
                clip: true
                contentWidth: width
                contentHeight: weightColumn.height
                visible: currentSpecies() !== 1

                Column {
                    id: weightColumn
                    width: weightFlick.width
                    spacing: 6

                    Repeater {
                        model: weightRowsModel(weightRefresh, currentSpecies())
                        delegate: Rectangle {
                            id: weightRow
                            property int rowIndex: modelData.index
                            width: weightColumn.width
                            height: 42
                            radius: 4
                            color: "#f8f8f8"
                            border.color: "#dddddd"
                            border.width: 1

                            Text {
                                x: 8
                                y: 5
                                width: parent.width - 158
                                height: 16
                                text: modelData.label
                                color: "#202020"
                                font.pixelSize: 11
                                font.bold: true
                                elide: Text.ElideRight
                            }

                            Text {
                                x: 8
                                y: 22
                                width: parent.width - 158
                                height: 14
                                text: "default " + signed(modelData.def)
                                color: "#777777"
                                font.pixelSize: 10
                                elide: Text.ElideRight
                            }

                            Text {
                                x: parent.width - 150
                                y: 12
                                width: 34
                                text: signed(modelData.value)
                                color: modelData.value < 0 ? "#8a3030" : "#205020"
                                font.pixelSize: 12
                                font.bold: true
                                horizontalAlignment: Text.AlignRight
                            }

                            Repeater {
                                model: [
                                    { label: "-10", delta: -10 },
                                    { label: "-",   delta: -1  },
                                    { label: "+",   delta: 1   },
                                    { label: "+10", delta: 10  }
                                ]
                                delegate: Rectangle {
                                    x: weightColumn.width - 108 + index * 28
                                    y: 9
                                    width: 24
                                    height: 24
                                    radius: 3
                                    color: "#ffffff"
                                    border.color: "#8c8c8c"

                                    Text {
                                        anchors.centerIn: parent
                                        text: modelData.label
                                        color: "#202020"
                                        font.pixelSize: 10
                                        font.bold: true
                                    }

                                    MouseArea {
                                        anchors.fill: parent
                                        onClicked: adjustWeightByIndex(weightRow.rowIndex, modelData.delta)
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        Rectangle {
            id: checkButton
            x: 18
            y: parent.height - 70
            width: 150
            height: 42
            radius: 4
            color: "#2f6fb3"

            Text {
                anchors.centerIn: parent
                text: "Check"
                color: "#ffffff"
                font.pixelSize: 15
                font.bold: true
            }

            MouseArea {
                anchors.fill: parent
                onClicked: runCheck()
            }
        }

        Rectangle {
            id: solveButton
            x: checkButton.x + checkButton.width + 12
            y: checkButton.y
            width: 180
            height: 42
            radius: 4
            color: "#ffffff"
            border.color: "#8c8c8c"

            Text {
                anchors.centerIn: parent
                text: "Solve selected"
                color: "#202020"
                font.pixelSize: 15
                font.bold: true
            }

            MouseArea {
                anchors.fill: parent
                onClicked: {
                    var keys = gatherSelectedKeys()
                    if (keys === null)
                        scoreLabel.text = scoreMarker + " no selection - select notes to solve"
                    else if (currentSpecies() > 2)
                        scoreLabel.text = scoreMarker + " solver supports species 1-2 for now"
                    else
                        runSolve(keys)
                }
            }
        }

        Rectangle {
            id: closeButton
            x: parent.width - width - 18
            y: checkButton.y
            width: 90
            height: 42
            radius: 4
            color: "#ffffff"
            border.color: "#8c8c8c"

            Text {
                anchors.centerIn: parent
                text: "Close"
                color: "#202020"
                font.pixelSize: 15
            }

            MouseArea {
                anchors.fill: parent
                onClicked: closeDialog()
            }
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
                pointDetailsText = "No score was returned. Make sure watch.sh is running, then click Check again."
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
            var solveResults = data.results || []
            applySolveResult(solveResults)
            var errors = 0
            for (var i = 0; i < solveResults.length; i++) {
                if (solveResults[i].error)
                    errors++
            }
            scoreLabel.text = scoreMarker + (errors > 0
                ? (" solve finished with " + errors + " error" + (errors === 1 ? "" : "s"))
                : " solve done")
            pointDetailsText = "Solve mode does not use the soft point score."
        } else {
            applyCheckResult(data.violations || [])
            var sc = (data.score !== undefined) ? (" — score " + data.score) : ""
            var n = (data.violations || []).length
            scoreLabel.text = scoreMarker + " " + n + " violation" +
                              (n === 1 ? "" : "s") + sc
            pointDetailsText = pointGuideForSpecies(currentSpecies(),
                                                    data.score,
                                                    data.violations || [],
                                                    data.points || [])
        }
    }

    function signed(n) {
        return n > 0 ? ("+" + n) : n.toString()
    }

    function pointGuideForSpecies(sp, score, violations, points) {
        var lines = []
        if (score === null || score === undefined)
            lines.push("Run Check to see the current score.")
        else
            lines.push("Current score: " + score)

        var schema = weightSchemas[sp.toString()]
        if (!schema || schema.length === 0) {
            lines.push("First species uses strict rules only; no soft-weight score is added.")
        } else {
            lines.push("Selected soft weights:")
            for (var i = 0; i < schema.length; i++)
                lines.push(signed(currentWeight(schema[i])) + "  " + schema[i].label)
        }

        if (points && points.length > 0) {
            lines.push("")
            lines.push("Point events:")
            var ordered = points.slice(0)
            ordered.sort(function(a, b) {
                var ap = (a.part === undefined) ? 0 : a.part
                var bp = (b.part === undefined) ? 0 : b.part
                if (ap !== bp) return ap - bp
                var as = (a.step === undefined) ? 0 : a.step
                var bs = (b.step === undefined) ? 0 : b.step
                return as - bs
            })
            var running = 0
            for (var k = 0; k < ordered.length; k++) {
                var p = ordered[k]
                var pts = (p.points === undefined) ? 0 : p.points
                running += pts
                var partText = (p.part !== undefined && p.part > 0)
                    ? ("part " + (p.part + 1) + ", ")
                    : ""
                var step = (p.step === undefined) ? 0 : p.step
                var rule = p.rule ? p.rule : "score"
                var detail = p.detail ? (" - " + p.detail) : ""
                lines.push(partText + "step " + step + ": " +
                           signed(pts) + " (total " + running + ") " +
                           rule + detail)
            }
        } else if (score !== null && score !== undefined && sp > 1) {
            lines.push("")
            lines.push("No soft-score point events were returned.")
        }

        if (violations && violations.length > 0) {
            lines.push("")
            lines.push("Warnings:")
            for (var j = 0; j < violations.length; j++) {
                var v = violations[j]
                lines.push("step " + v.step + ": " + abbreviate(v.rule) +
                           " - " + v.detail)
            }
        } else if (score !== null && score !== undefined) {
            lines.push("")
            lines.push("No hard-rule warnings were returned.")
        }

        lines.push("")
        lines.push("The total is computed by Lean from reward/penalty events; this panel shows the active weights and returned warnings.")
        return lines.join("\n")
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
            pointDetailsText = "Open a score before running Check."
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
                                voices: [v.voiceA, v.voiceB], rules: [] }
            } else {
                if (groups[key].voices.indexOf(v.voiceA) < 0)
                    groups[key].voices.push(v.voiceA)
                if (groups[key].voices.indexOf(v.voiceB) < 0)
                    groups[key].voices.push(v.voiceB)
            }
            if (groups[key].rules.indexOf(v.rule) < 0)
                groups[key].rules.push(v.rule)
        }
        for (var k in groups) {
            var g = groups[k]
            if (g.part >= map.length) continue
            var entry = map[g.part]
            var labelVoice = preferredLabelVoice(entry, g.voices)
            if (labelVoice >= entry.order.length) continue
            var msVoice = entry.order[labelVoice]
            var slotStep = visualStepForVoice(entry, labelVoice, g.step)
            var labels = []
            for (var r = 0; r < g.rules.length; r++)
                labels.push(abbreviate(g.rules[r]))
            addStatusAtStep(g.part, msVoice, slotStep,
                            statusMarker + labels.join("/"))
        }
    }

    function colorNote(map, partIdx, voiceIdx, stepIdx) {
        if (partIdx >= map.length) return
        var entry = map[partIdx]
        if (voiceIdx >= entry.order.length) return
        var msVoice = entry.order[voiceIdx]
        var slotStep = visualStepForVoice(entry, voiceIdx, stepIdx)
        var slotList = entry.slots[msVoice]
        if (!slotList || slotStep >= slotList.length) return
        var slot = slotList[slotStep]
        if (slot && slot.kind === "chord" && slot.note)
            slot.note.color = "#ff0000"
    }

    function subdivisionsForSpecies() {
        var sp = currentSpecies()
        if (sp === 3) return 4
        if (sp === 2 || sp === 4) return 2
        return 1
    }

    function isCfVoice(entry, voiceIdx) {
        if (voiceIdx >= entry.order.length) return false
        var mv = entry.order[voiceIdx]
        var len = entry.notes[mv] ? entry.notes[mv].length : 0
        var minLen = -1
        for (var i = 0; i < entry.order.length; i++) {
            var otherMv = entry.order[i]
            var otherLen = entry.notes[otherMv] ? entry.notes[otherMv].length : 0
            if (otherLen > 0 && (minLen < 0 || otherLen < minLen))
                minLen = otherLen
        }
        return minLen >= 0 && len === minLen
    }

    function visualStepForVoice(entry, voiceIdx, stepIdx) {
        if (currentSpecies() > 1 && currentSpecies() < 5 && isCfVoice(entry, voiceIdx))
            return Math.floor(stepIdx / subdivisionsForSpecies())
        return stepIdx
    }

    function preferredLabelVoice(entry, voices) {
        if (!voices || voices.length === 0) return 0
        if (currentSpecies() > 1) {
            for (var i = 0; i < voices.length; i++) {
                if (!isCfVoice(entry, voices[i]))
                    return voices[i]
            }
        }
        return voices[0]
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
    // Solve mode
    // -----------------------------------------------------------------------

    function runSolve(selectedKeys) {
        var map = buildVoiceMap()
        var cpInfo = findCpVoice(map, selectedKeys)
        if (cpInfo === null) { runCheck(); return }
        requestId = newRequestId()
        var parts = buildSolveJson(map, selectedKeys, cpInfo)
        var doc = {
            id: requestId,
            mode: "solve",
            species: currentSpecies(),
            parts: parts
        }
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
                    var selected = selectedKeys[staff + ":" + mv + ":" + slot.tick] === true
                    if (slot.kind === "rest") {
                        pitch = 0
                        free  = isCp && selected
                    } else {
                        pitch = slot.note.pitch
                        free  = isCp && selected
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
        addStatusTextAtCursor(cursor, msg, undefined)
    }

    function addStatusAtStep(staff, msVoice, step, msg) {
        var cursor = curScore.newCursor()
        cursor.rewind(0)
        cursor.staffIdx = staff
        cursor.voice = msVoice
        var count = 0
        while (cursor.segment) {
            if (cursor.element &&
                (cursor.element.type === Element.CHORD ||
                 cursor.element.type === Element.REST)) {
                if (count === step) {
                    addStatusTextAtCursor(cursor, msg, Placement.BELOW)
                    return
                }
                count++
            }
            cursor.next()
        }
    }

    function addStatusTextAtCursor(cursor, msg, placement) {
        removeStatusTextsFromSegment(cursor.segment)
        var text = newElement(Element.STAFF_TEXT)
        text.text = msg
        if (placement !== undefined)
            text.placement = placement
        cursor.add(text)
    }

    function isStatusTextElement(a) {
        if (!a || a.type !== Element.STAFF_TEXT || a.text === undefined)
            return false
        var t = "" + a.text
        for (var i = 0; i < statusTextFragments.length; i++) {
            if (t.indexOf(statusTextFragments[i]) >= 0)
                return true
        }
        return false
    }

    function removeStatusTextsFromSegment(segment) {
        if (!segment || !segment.annotations)
            return
        var anns = segment.annotations
        for (var i = anns.length - 1; i >= 0; i--) {
            var a = anns[i]
            if (isStatusTextElement(a))
                removeElement(a)
        }
    }

    function clearStatusTexts() {
        for (var staff = 0; staff < curScore.nstaves; staff++) {
            for (var msVoice = 0; msVoice < 4; msVoice++) {
                var cursor = curScore.newCursor()
                cursor.rewind(0)
                cursor.staffIdx = staff
                cursor.voice = msVoice
                while (cursor.segment) {
                    removeStatusTextsFromSegment(cursor.segment)
                    cursor.next()
                }
            }
        }
    }
}
