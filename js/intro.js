/* ════════════════════════════════════════════════════════════════
   LUVI CLAWNDESTINE — INTRO EXPERIENCE
   
   A ~37-second cinematic narrative about ALS clinical trials.
   250 particles represent simulated patients. They start scattered,
   converge into a single stream (standard analysis), then split
   into three colored trajectories (the LCMM revelation).
   
   Audio: Web Audio API synthesis — ambient drone, heartbeat, chord.
   No external audio files required.
   ════════════════════════════════════════════════════════════════ */

;(function () {
  'use strict'

  // ═══════════════════════════════════════════════════════
  // CONFIG
  // ═══════════════════════════════════════════════════════
  const PARTICLE_COUNT = window.innerWidth < 600 ? 150 : 250
  const COLORS = {
    slow:     [91, 155, 213],   // calm blue
    moderate: [232, 168, 56],   // warm amber
    fast:     [209, 75, 75],    // urgent red
    white:    [220, 216, 208],  // warm white
  }
  const CLASS_PROPS = [0.40, 0.35, 0.25] // slow, moderate, fast
  const SPRING = 0.025
  const DAMP   = 0.88
  const DPR = Math.min(window.devicePixelRatio || 1, 2)

  // ═══════════════════════════════════════════════════════
  // UTILITIES
  // ═══════════════════════════════════════════════════════
  const lerp  = (a, b, t) => a + (b - a) * t
  const clamp = (v, lo, hi) => Math.min(hi, Math.max(lo, v))
  const rand  = (lo, hi) => Math.random() * (hi - lo) + lo
  const easeOut4 = t => 1 - Math.pow(1 - t, 4)
  const easeInOut3 = t => t < 0.5 ? 4*t*t*t : 1 - Math.pow(-2*t+2,3)/2

  // ═══════════════════════════════════════════════════════
  // AUDIO ENGINE (Web Audio API — all synthesized)
  // ═══════════════════════════════════════════════════════
  class AudioEngine {
    constructor () {
      this.ctx = null
      this.master = null
      this.ambient = null
      this.heartbeatId = null
      this.chordOscs = []
    }

    init () {
      this.ctx = new (window.AudioContext || window.webkitAudioContext)()
      this.master = this.ctx.createGain()
      this.master.gain.value = 0
      this.master.connect(this.ctx.destination)
    }

    // Low ambient drone — two detuned triangles through a lowpass
    startAmbient () {
      const now = this.ctx.currentTime
      const g = this.ctx.createGain()
      g.gain.setValueAtTime(0, now)
      g.gain.linearRampToValueAtTime(0.06, now + 4)

      const lp = this.ctx.createBiquadFilter()
      lp.type = 'lowpass'
      lp.frequency.setValueAtTime(180, now)

      const o1 = this.ctx.createOscillator()
      const o2 = this.ctx.createOscillator()
      o1.type = o2.type = 'triangle'
      o1.frequency.value = 55
      o2.frequency.value = 55.4
      o1.connect(lp); o2.connect(lp)
      lp.connect(g); g.connect(this.master)
      o1.start(); o2.start()

      this.master.gain.setValueAtTime(0.8, now)
      this.master.gain.linearRampToValueAtTime(1, now + 6)

      this.ambient = { g, lp, oscs: [o1, o2] }
    }

    swell (targetGain, filterFreq, dur) {
      if (!this.ambient) return
      const now = this.ctx.currentTime
      this.ambient.g.gain.linearRampToValueAtTime(targetGain, now + dur)
      this.ambient.lp.frequency.linearRampToValueAtTime(filterFreq, now + dur)
    }

    // Heartbeat — double-pulse (lub-dub)
    startHeartbeat () {
      const beat = () => {
        if (!this.ctx) return
        const now = this.ctx.currentTime
        const play = (offset) => {
          const o = this.ctx.createOscillator()
          const g = this.ctx.createGain()
          o.type = 'sine'
          o.frequency.value = 48
          g.gain.setValueAtTime(0, now + offset)
          g.gain.linearRampToValueAtTime(0.18, now + offset + 0.04)
          g.gain.exponentialRampToValueAtTime(0.001, now + offset + 0.25)
          o.connect(g); g.connect(this.master)
          o.start(now + offset)
          o.stop(now + offset + 0.3)
        }
        play(0)
        play(0.14)
      }
      beat()
      this.heartbeatId = setInterval(beat, 1100)
    }

    stopHeartbeat () {
      if (this.heartbeatId) { clearInterval(this.heartbeatId); this.heartbeatId = null }
    }

    playSingleTone () {
      const now = this.ctx.currentTime
      const o = this.ctx.createOscillator()
      const g = this.ctx.createGain()
      o.type = 'triangle'
      o.frequency.value = 130.81
      g.gain.setValueAtTime(0, now)
      g.gain.linearRampToValueAtTime(0.08, now + 1.5)
      o.connect(g); g.connect(this.master)
      o.start()
      this.chordOscs.push({ o, g })
    }

    playChord () {
      const now = this.ctx.currentTime
      if (this.chordOscs[0]) {
        this.chordOscs[0].g.gain.linearRampToValueAtTime(0.1, now + 1)
      }
      ;[164.81, 196.00].forEach(freq => {
        const o = this.ctx.createOscillator()
        const g = this.ctx.createGain()
        o.type = 'triangle'
        o.frequency.value = freq
        g.gain.setValueAtTime(0, now)
        g.gain.linearRampToValueAtTime(0.08, now + 1.5)
        o.connect(g); g.connect(this.master)
        o.start()
        this.chordOscs.push({ o, g })
      })
    }

    fadeOut (dur) {
      if (!this.ctx) return
      const now = this.ctx.currentTime
      this.master.gain.linearRampToValueAtTime(0, now + dur)
      this.stopHeartbeat()
      setTimeout(() => {
        this.chordOscs.forEach(c => { try { c.o.stop() } catch(e){} })
        if (this.ambient) this.ambient.oscs.forEach(o => { try { o.stop() } catch(e){} })
        try { this.ctx.close() } catch(e){}
        this.ctx = null
      }, dur * 1000 + 200)
    }
  }

  // ═══════════════════════════════════════════════════════
  // PARTICLE
  // ═══════════════════════════════════════════════════════
  class Particle {
    constructor (canvas, classIdx) {
      this.canvas = canvas
      this.classIdx = classIdx
      this.x = rand(0, canvas.width / DPR)
      this.y = rand(0, canvas.height / DPR)
      this.tx = this.x; this.ty = this.y
      this.vx = 0; this.vy = 0
      this.size = rand(1.2, 2.8)
      this.alpha = 0
      this.targetAlpha = 0.6
      this.r = COLORS.white[0]; this.g = COLORS.white[1]; this.b = COLORS.white[2]
      this.tr = this.r; this.tg = this.g; this.tb = this.b
      this.noise = rand(-20, 20)
      this.flowSpeed = rand(0.6, 1.2)
      this.phase = rand(0, Math.PI * 2)
    }

    setTargetColor (rgb) {
      this.tr = rgb[0]; this.tg = rgb[1]; this.tb = rgb[2]
    }

    update (mode, splitProgress, time) {
      const w = this.canvas.width / DPR
      const h = this.canvas.height / DPR
      const cx = w * 0.5, cy = h * 0.5

      this.alpha += (this.targetAlpha - this.alpha) * 0.05
      this.r += (this.tr - this.r) * 0.03
      this.g += (this.tg - this.g) * 0.03
      this.b += (this.tb - this.b) * 0.03

      if (mode === 'void') {
        this.vx += rand(-0.03, 0.03)
        this.vy += rand(-0.03, 0.03)
        this.vx *= 0.98; this.vy *= 0.98
        this.x += this.vx; this.y += this.vy
        if (this.x < 0) this.vx += 0.1
        if (this.x > w) this.vx -= 0.1
        if (this.y < 0) this.vy += 0.1
        if (this.y > h) this.vy -= 0.1
        return
      }

      if (mode === 'converge' || mode === 'stream' || mode === 'split') {
        const singleY = cy + Math.sin(this.x * 0.008 + this.phase) * 12 + this.noise * 0.5
        const offsets = [-1, 0, 1]
        const xNorm = clamp(this.x / w, 0, 1)
        const baseSpread = xNorm * h * 0.13
        const curvatures = [0, 0.03, 0.09]
        const splitY = cy
          + offsets[this.classIdx] * baseSpread
          + curvatures[this.classIdx] * xNorm * h
          + Math.sin(this.x * 0.007 + this.classIdx * 2.1 + this.phase) * 7
          + this.noise * 0.3

        const sp = mode === 'split' ? clamp(splitProgress, 0, 1) : 0
        this.ty = lerp(singleY, splitY, easeInOut3(sp))

        if (mode === 'stream' || mode === 'split') {
          this.x += this.flowSpeed
          if (this.x > w + 10) this.x = -10
        } else {
          this.vx += (this.tx - this.x) * SPRING * 0.5
          this.vx *= DAMP
          this.x += this.vx
        }

        this.vy += (this.ty - this.y) * SPRING
        this.vy *= DAMP
        this.y += this.vy
        return
      }

      if (mode === 'dissolve') {
        const dx = this.x - cx, dy = this.y - cy
        const dist = Math.sqrt(dx*dx + dy*dy) || 1
        this.vx += (dx / dist) * 0.04
        this.vy += (dy / dist) * 0.04
        this.vx *= 0.995; this.vy *= 0.995
        this.x += this.vx; this.y += this.vy
        this.targetAlpha = Math.max(0, this.targetAlpha - 0.008)
        return
      }
    }
  }

  // ═══════════════════════════════════════════════════════
  // RENDERER
  // ═══════════════════════════════════════════════════════
  class Renderer {
    constructor (canvas) {
      this.canvas = canvas
      this.ctx = canvas.getContext('2d')
      this.resize()
    }

    resize () {
      const w = window.innerWidth, h = window.innerHeight
      this.canvas.width = w * DPR
      this.canvas.height = h * DPR
      this.canvas.style.width = w + 'px'
      this.canvas.style.height = h + 'px'
      this.ctx.setTransform(DPR, 0, 0, DPR, 0, 0)
    }

    clear () {
      this.ctx.clearRect(0, 0, this.canvas.width / DPR, this.canvas.height / DPR)
    }

    drawParticles (particles) {
      const ctx = this.ctx
      ctx.globalCompositeOperation = 'lighter'
      for (const p of particles) {
        if (p.alpha < 0.01) continue
        ctx.fillStyle = `rgba(${p.r|0},${p.g|0},${p.b|0},${p.alpha * 0.1})`
        ctx.beginPath()
        ctx.arc(p.x, p.y, p.size * 4, 0, Math.PI * 2)
        ctx.fill()
        ctx.fillStyle = `rgba(${p.r|0},${p.g|0},${p.b|0},${p.alpha * 0.75})`
        ctx.beginPath()
        ctx.arc(p.x, p.y, p.size, 0, Math.PI * 2)
        ctx.fill()
      }
      ctx.globalCompositeOperation = 'source-over'
    }

    drawPulse (cx, cy, progress) {
      if (progress <= 0 || progress >= 1) return
      const maxR = Math.max(window.innerWidth, window.innerHeight) * 0.7
      const r = maxR * easeOut4(progress)
      const alpha = 0.25 * (1 - progress)
      this.ctx.strokeStyle = `rgba(255,255,255,${alpha})`
      this.ctx.lineWidth = 1.5
      this.ctx.beginPath()
      this.ctx.arc(cx, cy, r, 0, Math.PI * 2)
      this.ctx.stroke()
    }

    drawCurves (splitProgress, canvas) {
      if (splitProgress <= 0) return
      const ctx = this.ctx
      const w = canvas.width / DPR, h = canvas.height / DPR
      const cy = h * 0.5
      const colors = [COLORS.slow, COLORS.moderate, COLORS.fast]
      const offsets = [-1, 0, 1]
      const curvatures = [0, 0.03, 0.09]
      for (let c = 0; c < 3; c++) {
        const alpha = clamp(splitProgress, 0, 1) * 0.12
        ctx.strokeStyle = `rgba(${colors[c][0]},${colors[c][1]},${colors[c][2]},${alpha})`
        ctx.lineWidth = 1
        ctx.beginPath()
        for (let px = 0; px <= w; px += 3) {
          const xNorm = px / w
          const baseSpread = xNorm * h * 0.13
          const y = cy + offsets[c] * baseSpread + curvatures[c] * xNorm * h
          if (px === 0) ctx.moveTo(px, y)
          else ctx.lineTo(px, y)
        }
        ctx.stroke()
      }
    }
  }

  // ═══════════════════════════════════════════════════════
  // TEXT CONTROLLER
  // ═══════════════════════════════════════════════════════
  class TextCtrl {
    constructor (layer) {
      this.layer = layer
      this.els = {}
    }

    show (id, html, css, style) {
      let el = this.els[id]
      if (!el) {
        el = document.createElement('div')
        el.className = 'it ' + (css || '')
        this.layer.appendChild(el)
        this.els[id] = el
      }
      el.className = 'it ' + (css || '')
      el.innerHTML = html
      if (style) Object.assign(el.style, style)
      void el.offsetWidth
      requestAnimationFrame(() => el.classList.add('v'))
    }

    hide (id) {
      const el = this.els[id]
      if (!el) return
      el.classList.remove('v')
      el.classList.add('out')
    }

    hideAll () { Object.keys(this.els).forEach(id => this.hide(id)) }
    remove (id) { const el = this.els[id]; if (el) { el.remove(); delete this.els[id] } }
    removeAll () { Object.keys(this.els).forEach(id => this.remove(id)) }
  }

  // ═══════════════════════════════════════════════════════
  // MAIN EXPERIENCE
  // ═══════════════════════════════════════════════════════
  class Experience {
    constructor () {
      this.overlay = document.getElementById('intro')
      if (!this.overlay) return

      this.canvas = document.getElementById('intro-canvas')
      this.renderer = new Renderer(this.canvas)
      this.textCtrl = new TextCtrl(document.getElementById('intro-text'))
      this.audio = new AudioEngine()
      this.particles = []
      this.mode = 'void'
      this.time = 0
      this.started = false
      this.frozen = false
      this.frozenUntil = 0
      this.splitProgress = 0
      this.pulseTime = -1
      this.ending = false
      this.lastFrame = 0
      this.spawned = 0

      document.body.classList.add('intro-active')
      this._spawnPreviewParticles(12)

      document.getElementById('intro-gate').addEventListener('click', () => this.start())
      document.getElementById('intro-skip').addEventListener('click', () => this.skip())
      window.addEventListener('resize', () => this.renderer.resize())

      this._raf = requestAnimationFrame(t => this._gateLoop(t))
    }

    _spawnPreviewParticles (n) {
      const w = this.canvas.width / DPR, h = this.canvas.height / DPR
      for (let i = 0; i < n; i++) {
        const ci = i < n * 0.4 ? 0 : i < n * 0.75 ? 1 : 2
        const p = new Particle(this.canvas, ci)
        p.x = w/2 + rand(-40, 40)
        p.y = h/2 + rand(-40, 40)
        p.targetAlpha = 0.25
        p.size = rand(1, 2)
        this.particles.push(p)
      }
    }

    _gateLoop (ts) {
      if (this.started) return
      this.renderer.clear()
      for (const p of this.particles) p.update('void', 0, 0)
      this.renderer.drawParticles(this.particles)
      this._raf = requestAnimationFrame(t => this._gateLoop(t))
    }

    // ── TIMELINE ──
    _buildTimeline () {
      const T = []
      const at = (sec, fn) => T.push({ t: sec, fn, done: false })
      const ctr = { top: '42%', left: '50%', transform: 'translate(-50%,-50%)' }
      const ctrLow = { top: '52%', left: '50%', transform: 'translate(-50%,-50%)' }

      // 0.0 — Ambient
      at(0, () => this.audio.startAmbient())

      // 1.5 — Stakes
      at(1.5, () => {
        this.textCtrl.show('a',
          'Every 90 minutes,<br>someone is diagnosed with ALS.',
          'it--narr', ctr)
        this._spawnTo(80)
      })

      at(5.0, () => this.textCtrl.hide('a'))

      // 5.5 — Failure
      at(5.5, () => {
        this.textCtrl.show('b',
          'No cure.<br>70+ drugs have failed in clinical trials.',
          'it--narr', ctr)
        this._spawnTo(PARTICLE_COUNT)
        this.audio.startHeartbeat()
        this.audio.swell(0.09, 240, 3)
      })

      at(8.5, () => this.textCtrl.hide('b'))

      // 9.5 — The question
      at(9.5, () => {
        this.textCtrl.show('c',
          'What if the problem isn\'t the drugs?',
          'it--emp', ctr)
        this.audio.swell(0.12, 300, 2.5)
      })

      at(12.0, () => {
        this.textCtrl.hide('c')
        this.mode = 'converge'
        this.audio.stopHeartbeat()
        const w = this.canvas.width / DPR
        this.particles.forEach(p => { p.tx = rand(0, w) })
      })

      // 12.5 — The computation (AI element)
      at(12.5, () => {
        this.textCtrl.show('n',
          '<span class="comp-num">14,650</span>' +
          '<span class="comp-label">simulated trials · run by an AI</span>',
          'it--comp', ctr)
      })

      at(15.5, () => {
        this.textCtrl.hide('n')
        this.mode = 'stream'
      })

      // 16.0 — One story
      at(16.0, () => {
        this.textCtrl.show('d',
          'Standard methods see one story.',
          'it--narr', ctr)
        this.audio.playSingleTone()
      })

      at(19.0, () => this.textCtrl.hide('d'))

      // 20.0 — FREEZE + SPLIT
      at(20.0, () => {
        this.frozen = true
        this.frozenUntil = this.time + 0.35
      })

      at(20.35, () => {
        this.frozen = false
        this.mode = 'split'
        this.splitStart = this.time
        this.pulseTime = 0
        this.textCtrl.show('e', 'We found three.', 'it--emp', ctr)
        this.audio.playChord()
        this.audio.swell(0.14, 400, 2)
        const cls = [COLORS.slow, COLORS.moderate, COLORS.fast]
        this.particles.forEach(p => p.setTargetColor(cls[p.classIdx]))
      })

      // 23.0 — Stats
      at(23.0, () => {
        this.textCtrl.hide('e')
        this.textCtrl.show('s1', '12%', 'it--stat it--stat-dim',
          { top: '38%', left: '32%', transform: 'translate(-50%,-50%)' })
        this.textCtrl.show('s2', '90%', 'it--stat it--stat-bright',
          { top: '38%', left: '68%', transform: 'translate(-50%,-50%)' })
        this.textCtrl.show('s3', 'Same patients. Same data. A different lens.', 'it--sub',
          { top: '52%', left: '50%', transform: 'translate(-50%,-50%)' })
      })

      at(26.0, () => {
        this.textCtrl.hide('s1')
        this.textCtrl.hide('s2')
        this.textCtrl.hide('s3')
      })

      // 27.0 — Title + AI Identity
      at(27.0, () => {
        this.mode = 'dissolve'
        this.textCtrl.show('title', 'Adversarial Science', 'it--title',
          { top: '34%', left: '50%', transform: 'translate(-50%,-50%)' })
      })

      at(28.5, () => {
        this.textCtrl.show('identity',
          'Luvi Clawndestine · AI Research Agent',
          'it--identity',
          { top: '46%', left: '50%', transform: 'translate(-50%,-50%)' })
      })

      at(29.5, () => {
        this.textCtrl.show('tagline',
          'We audit the assumptions nobody questions.',
          'it--sub',
          { top: '53%', left: '50%', transform: 'translate(-50%,-50%)' })
      })

      // 32.0 — CTAs
      at(32.0, () => {
        this.textCtrl.show('cta', `
          <div class="intro-ctas">
            <a href="/lab/" class="intro-cta">Enter the Lab</a>
            <a href="/preprint/" class="intro-cta">Read the Preprint</a>
            <a href="#" class="intro-cta" id="intro-home">Homepage</a>
          </div>
        `, '', { top: '64%', left: '50%', transform: 'translate(-50%,-50%)' })

        setTimeout(() => {
          const home = document.getElementById('intro-home')
          if (home) home.addEventListener('click', e => { e.preventDefault(); this.end() })
        }, 100)
      })

      // 37.0 — Auto-dismiss
      at(37.0, () => this.end())

      return T
    }

    _spawnTo (target) {
      const diff = target - this.spawned
      if (diff <= 0) return
      const w = this.canvas.width / DPR, h = this.canvas.height / DPR
      for (let i = 0; i < diff; i++) {
        const r = Math.random()
        const ci = r < CLASS_PROPS[0] ? 0 : r < CLASS_PROPS[0]+CLASS_PROPS[1] ? 1 : 2
        const p = new Particle(this.canvas, ci)
        p.x = rand(0, w)
        p.y = rand(0, h)
        p.targetAlpha = rand(0.35, 0.7)
        this.particles.push(p)
      }
      this.spawned = target
    }

    start () {
      if (this.started) return
      this.started = true
      cancelAnimationFrame(this._raf)

      const gate = document.getElementById('intro-gate')
      gate.style.opacity = '0'
      gate.style.transition = 'opacity 0.4s'
      setTimeout(() => gate.remove(), 500)

      setTimeout(() => {
        const skip = document.getElementById('intro-skip')
        if (skip) skip.classList.add('show')
      }, 2500)

      this.particles.forEach(p => { p.targetAlpha = rand(0.35, 0.7) })
      this.audio.init()
      this.timeline = this._buildTimeline()
      this.lastFrame = performance.now()
      this._raf = requestAnimationFrame(ts => this.loop(ts))
    }

    skip () { this.end() }

    end () {
      if (this.ending) return
      this.ending = true
      sessionStorage.setItem('luvi-intro-seen', '1')
      this.textCtrl.removeAll()
      this.audio.fadeOut(1.5)
      document.body.classList.add('intro-ending')
      this.overlay.classList.add('ending')
      setTimeout(() => {
        cancelAnimationFrame(this._raf)
        this.overlay.remove()
        document.body.classList.remove('intro-active', 'intro-ending')
      }, 1600)
    }

    loop (ts) {
      if (this.ending) {
        this.renderer.clear()
        this.particles.forEach(p => p.update('dissolve', 0, this.time))
        this.renderer.drawParticles(this.particles)
        this._raf = requestAnimationFrame(t => this.loop(t))
        return
      }

      const dt = Math.min((ts - this.lastFrame) / 1000, 0.05)
      this.lastFrame = ts
      this.time += dt

      for (const ev of this.timeline) {
        if (!ev.done && this.time >= ev.t) { ev.fn(); ev.done = true }
      }

      if (this.mode === 'split' && this.splitStart !== undefined) {
        this.splitProgress = clamp((this.time - this.splitStart) / 2.5, 0, 1)
      }

      if (this.pulseTime >= 0) {
        this.pulseTime += dt
        if (this.pulseTime > 1.5) this.pulseTime = -1
      }

      if (!this.frozen) {
        for (const p of this.particles) {
          p.update(this.mode, this.splitProgress, this.time)
        }
      }

      this.renderer.clear()
      this.renderer.drawCurves(this.splitProgress, this.canvas)
      this.renderer.drawParticles(this.particles)

      if (this.pulseTime >= 0) {
        this.renderer.drawPulse(
          window.innerWidth / 2,
          window.innerHeight / 2,
          this.pulseTime / 1.5
        )
      }

      this._raf = requestAnimationFrame(t => this.loop(t))
    }
  }

  // ═══════════════════════════════════════════════════════
  // INIT
  // ═══════════════════════════════════════════════════════
  function init () {
    // Skip intro if already seen this session
    if (sessionStorage.getItem('luvi-intro-seen')) {
      const el = document.getElementById('intro')
      if (el) el.remove()
      return
    }
    new Experience()
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init)
  } else {
    init()
  }
})()
