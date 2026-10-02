// GTA V Takeover: the test chamber turns into a wanted-level chase.
// Real 3D models (police cars, taxis, cash stacks, HUD panels) made for this mod: models/gta/*.mdl.
// Cops (turrets in police cars) arrive, taxis ram them, every cop that falls gets WASTED, then MISSION PASSED and cash rains.

::GTA <- {
	cash = 0, wanted = 0, flip = false, wave = 0, busy = false, t0 = 0.0, fired = {}, pitch = 8.0,
	hud = null, hudReady = false, cops = [], taxis = [], cash_stacks = [], wrecks = [], dots = [], logoUntil = 0.0,
	bannerUntil = 0.0, bannerEnt = null
}

// the kit only remembers the view yaw: remember the pitch too (the HUD is glued to the view)
::SigfLookRaw <- ::SigfLook
::SigfLook <- function(pitch, yaw) {
	::GTA.pitch = pitch
	::SigfLookRaw(pitch, yaw)
}

::GtaOk <- function(e) { return e != null && e.IsValid() }

::GtaFmt <- function(n) {
	local s = n.tostring()
	local out = ""
	local c = 0
	for (local i = s.len() - 1; i >= 0; i--) {
		out = s.slice(i, i + 1) + out
		c++
		if (c % 3 == 0 && i > 0) out = "," + out
	}
	return out
}

// ---------- model spawning (retries when the game refuses to place it) ----------

::GtaModel <- function(model, fn, tries = 5) {
	local st = { done = false }
	SigfDynamic(model, SigfHost().EyePosition(), 0.0, function(e):(st, fn) {
		if (st.done) { e.Destroy(); return }
		st.done = true
		fn(e)
	})
	if (tries > 0) {
		SigfIn(0.6, function():(model, fn, st, tries) {
			if (!st.done) ::GtaModel(model, fn, tries - 1)
		})
	}
}

::GtaAway <- Vector(0, 0, -30000)

// ---------- HUD: real flat models glued in front of the eyes ----------

::GtaView <- function() {
	local host = SigfHost()
	local f = SigfForward()
	local yaw = atan2(f.y, f.x)
	local p = ::GTA.pitch / 57.29578
	return {
		eye = host.EyePosition(), yaw = yaw * 57.29578, pitch = ::GTA.pitch,
		fwd = Vector(cos(p) * cos(yaw), cos(p) * sin(yaw), -sin(p)),
		right = Vector(sin(yaw), -cos(yaw), 0.0),
		up = Vector(sin(p) * cos(yaw), sin(p) * sin(yaw), cos(p))
	}
}

// slot = { ent, r, u, d, on }
::GtaPlace <- function(v, slot) {
	if (slot.ent == null || !slot.ent.IsValid()) return
	if (!slot.on) { slot.ent.SetOrigin(::GtaAway); return }
	slot.ent.SetOrigin(v.eye + v.fwd * slot.d + v.right * slot.r + v.up * slot.u)
	slot.ent.SetAngles(v.pitch, v.yaw, 0.0)
}

::GtaHudTick <- function() {
	local h = ::GTA.hud
	if (h == null || SigfHost() == null) return
	local v = ::GtaView()
	local now = Time()
	h.radar.on = true
	h.logo.on = now < ::GTA.logoUntil
	h.wasted.on = ::GTA.bannerEnt == "wasted" && now < ::GTA.bannerUntil
	h.passed.on = ::GTA.bannerEnt == "passed" && now < ::GTA.bannerUntil
	for (local i = 0; i < 5; i++) h.wanted[i].on = (::GTA.wanted == i + 1)
	::GtaPlace(v, h.radar)
	::GtaPlace(v, h.logo)
	::GtaPlace(v, h.wasted)
	::GtaPlace(v, h.passed)
	foreach (w in h.wanted) ::GtaPlace(v, w)
	::GtaRadar(v)
}

// minimap: every cop (red dot) and taxi (yellow dot) shows up, the player is the arrow in the middle, the view points up
::GtaDot <- function(v, slot, pos, me) {
	local h = ::GTA.hud
	if (slot.ent == null || !slot.ent.IsValid()) return
	if (pos == null) { slot.on = false; ::GtaPlace(v, slot); return }
	local rel = pos - me
	local fl = Vector(v.fwd.x, v.fwd.y, 0.0)
	fl.Norm()
	local sx = rel.Dot(v.right) * 0.028
	local sy = rel.Dot(fl) * 0.028
	local len = sqrt(sx * sx + sy * sy)
	if (len > 16.0) { sx = sx * 16.0 / len; sy = sy * 16.0 / len }
	slot.on = h.radar.on
	slot.r = h.radar.r + sx
	slot.u = h.radar.u + sy
	slot.d = h.radar.d - 0.4
	::GtaPlace(v, slot)
}

::GtaRadar <- function(v) {
	local h = ::GTA.hud
	local me = SigfHost().GetOrigin()
	for (local i = 0; i < 5; i++) {
		local p = null
		if (i < ::GTA.cops.len() && ::GtaOk(::GTA.cops[i].turret) && !::GTA.cops[i].dead) p = ::GTA.cops[i].turret.GetOrigin()
		::GtaDot(v, h.dots[i], p, me)
	}
	for (local i = 0; i < 2; i++) {
		local p = null
		if (i < ::GTA.taxis.len() && ::GtaOk(::GTA.taxis[i].car)) p = ::GTA.taxis[i].car.GetOrigin()
		::GtaDot(v, h.dots[5 + i], p, me)
	}
}

::GtaSlot <- function(r, u, d) { return { ent = null, r = r, u = u, d = d, on = false } }

::GtaBuildHud <- function() {
	local h = { radar = ::GtaSlot(-67.0, -28.0, 70.0), logo = ::GtaSlot(-22.0, 38.0, 70.0),
		wasted = ::GtaSlot(0.0, 6.0, 70.0), passed = ::GtaSlot(0.0, 6.0, 70.0), wanted = [], dots = [] }
	for (local i = 0; i < 5; i++) h.wanted.append(::GtaSlot(52.0, 42.0, 70.0))
	for (local i = 0; i < 7; i++) h.dots.append(::GtaSlot(0.0, 0.0, 70.0))
	local jobs = [
		["models/gta/h_radar.mdl", h.radar], ["models/gta/h_logo.mdl", h.logo], ["models/gta/h_wasted_b.mdl", h.wasted],
		["models/gta/h_passed_b.mdl", h.passed]
	]
	for (local i = 0; i < 5; i++) jobs.append(["models/gta/h_wanted" + (i + 1) + ".mdl", h.wanted[i]])
	for (local i = 0; i < 7; i++) jobs.append([i < 5 ? "models/gta/h_dot_r.mdl" : "models/gta/h_dot_y.mdl", h.dots[i]])
	for (local i = 0; i < jobs.len(); i++) {
		local job = jobs[i]
		SigfIn(i * 0.12, function():(job) {
			::GtaModel(job[0], function(e):(job) { job[1].ent = e; e.SetOrigin(::GtaAway) })
		})
	}
	::GTA.hud = h
	SigfIn(jobs.len() * 0.12 + 1.2, function():(jobs) {
		local miss = 0
		foreach (j in jobs) { if (j[1].ent == null) { miss++; printl("GTA dbg missing hud model " + j[0]) } }
		printl("GTA dbg hud ready, missing " + miss + " of " + jobs.len())
		::GTA.hudReady = true
		::GtaRestart()
	})
}

::GtaCash <- function(add) {
	::GTA.cash += add
	SigfText("$ " + ::GtaFmt(::GTA.cash), 0.04, 0.05, 1.2, "90 230 90", 2)
}

// ---------- full-screen flashes (env_fade) ----------

::GtaFade <- function(rgb, amt, dur, hold, flags) {
	local f = Entities.CreateByClassname("env_fade")
	f.__KeyValueFromString("rendercolor", rgb)
	f.__KeyValueFromInt("renderamt", amt)
	f.__KeyValueFromFloat("duration", dur)
	f.__KeyValueFromFloat("holdtime", hold)
	f.__KeyValueFromInt("spawnflags", flags)
	EntFireByHandle(f, "Fade", "", 0.0, null, null)
	EntFireByHandle(f, "Kill", "", dur + hold + 1.0, null, null)
}

// police lights: short, strong red and blue flashes over the whole screen
::GtaLights <- function() {
	::GTA.flip = !::GTA.flip
	if (::GTA.busy || ::GTA.wanted <= 0) return
	::GtaFade(::GTA.flip ? "255 15 15" : "20 60 255", 95, 0.16, 0.0, 1)
	foreach (c in ::GTA.cops) {
		if (::GtaOk(c.turret)) SigfColor(c.turret, ::GTA.flip ? "255 60 60" : "70 110 255")
	}
}

// ---------- places ----------

// A floor spot in front of the player: yaw offset in degrees, distance in units.
::GtaSpot <- function(off, dist) {
	local host = SigfHost()
	local f = SigfForward()
	local feet = host.GetOrigin()
	local eye = host.EyePosition()
	for (local k = 0; k < 8; k++) {
		local a = atan2(f.y, f.x) + (off + k * 45.0) / 57.29578
		local dir = Vector(cos(a), sin(a), 0.0)
		local frac = TraceLine(eye, eye + dir * dist, host)
		if (frac < 0.55) continue
		local p = ::SigfFloorAt(eye + dir * (dist * frac * 0.8))
		if (p != null && fabs(p.z - feet.z) < 60.0) return p
	}
	return SigfGround()
}

::GtaYawTo <- function(from, to) {
	return atan2(to.y - from.y, to.x - from.x) * 57.29578
}

// ---------- cops: a turret inside a police car ----------

::GtaCopCount <- function() { return ::GTA.wanted + 2 > 5 ? 5 : ::GTA.wanted + 2 }

::GtaSpawnCop <- function(i, n) {
	local host = SigfHost()
	local pos = ::GtaSpot((i - (n - 1) / 2.0) * 26.0, 290.0 + (i % 2) * 70.0)
	local yaw = ::GtaYawTo(pos, host.GetOrigin())
	local cop = { turret = null, car = null, pos = pos, yaw = yaw, dead = false }
	::GTA.cops.append(cop)
	SigfTurret(pos, yaw, 60.0, function(t):(cop) { cop.turret = t })
	::GtaModel("models/gta/copcar.mdl", function(e):(cop, pos, yaw) {
		cop.car = e
		EntFireByHandle(e, "DisableCollision", "", 0.0, null, null)
		e.SetOrigin(pos)
		e.SetAngles(0.0, yaw, 0.0)
	})
}

::GtaCops <- function() {
	::GTA.wave++
	::GTA.wanted = ::GTA.wave * 2 - 1 > 5 ? 5 : ::GTA.wave * 2 - 1
	SigfSound("sigf/siren.wav")
	SigfText("Cops are everywhere", -1.0, 0.16, 3.0, "255 80 80", 0)
	::GTA.cops = []
	local n = ::GtaCopCount()
	for (local i = 0; i < n; i++) {
		SigfIn(i * 0.35, function():(i, n) { ::GtaSpawnCop(i, n) })
	}
}

// the police car of a fallen cop flips through the air, then disappears
::GtaWreck <- function(cop) {
	if (cop.car == null || !cop.car.IsValid()) return
	::GTA.wrecks.append({ ent = cop.car, t = 0.0, pos = cop.car.GetOrigin(), yaw = cop.yaw, spin = RandomFloat(-1.0, 1.0) > 0 ? 1.0 : -1.0, life = 2.4 })
	cop.car = null
}

::GtaKill <- function(i) {
	if (i >= ::GTA.cops.len()) return
	local cop = ::GTA.cops[i]
	if (cop.dead) return
	cop.dead = true
	if (cop.turret != null && cop.turret.IsValid()) EntFireByHandle(cop.turret, "SelfDestruct", "", 0.0, null, null)
	::GtaWreck(cop)
	::GtaCash(250)
	::GtaWasted()
}

// ---------- taxis ----------

::GtaTaxi <- function(i, secs) {
	if (i >= ::GTA.cops.len()) return
	local cop = ::GTA.cops[i]
	local side = (i % 2 == 0) ? -62.0 : 62.0
	local from = ::GtaSpot(side, 420.0)
	local to = cop.pos
	local yaw = ::GtaYawTo(from, to)
	local t = { car = null, from = from, to = to, yaw = yaw, t = 0.0, secs = secs, i = i, hit = false, out = 0.0 }
	::GTA.taxis.append(t)
	::GtaModel("models/gta/taxi.mdl", function(e):(t, from, yaw) {
		t.car = e
		EntFireByHandle(e, "DisableCollision", "", 0.0, null, null)
		e.SetOrigin(from)
		e.SetAngles(0.0, yaw, 0.0)
		SigfSound("sigf/honk.wav")
	})
}

// ---------- cash rain: real cash-stack models ----------

::GtaRain <- function(count) {
	local host = SigfHost()
	local base = host.GetOrigin()
	for (local i = 0; i < count; i++) {
		SigfIn(i * 0.09, function():(base) {
			local pos = ::GtaSpot(RandomFloat(-35.0, 35.0), RandomFloat(150.0, 380.0))
			local top = pos + Vector(0, 0, RandomFloat(230.0, 330.0))
			::GtaModel("models/gta/cash.mdl", function(e):(pos, top) {
				e.SetOrigin(top)
				::GTA.cash_stacks.append({ ent = e, x = top.x, y = top.y, z = top.z, floor = pos.z + 10.0, t = 0.0, yaw = RandomFloat(0.0, 360.0),
					spin = RandomFloat(120.0, 300.0), tilt = RandomFloat(-40.0, 40.0), landed = 0.0, v = RandomFloat(150.0, 230.0) })
			})
		})
	}
}

// ---------- per-frame animation ----------

::GtaFast <- function() {
	local dt = 0.03
	::GtaHudTick()
	// cop cars follow their turret
	foreach (c in ::GTA.cops) {
		if (c.car != null && c.car.IsValid()) {
			if (c.turret != null && c.turret.IsValid()) { local tp = c.turret.GetOrigin(); c.car.SetOrigin(Vector(tp.x, tp.y, c.pos.z)) }
			else if (c.turret != null && !c.dead) { c.dead = true; ::GtaWreck(c) }
		}
	}
	// taxis drive at the cop they are after, then crash
	local keep = []
	foreach (t in ::GTA.taxis) {
		if (t.car == null || !t.car.IsValid()) { if (t.car == null) keep.append(t); continue }
		t.t += dt
		if (t.t < t.secs) {
			local k = t.t / t.secs
			local p = t.from + (t.to - t.from) * k
			p.z = p.z + fabs(sin(t.t * 16.0)) * 2.5
			t.car.SetOrigin(p)
			t.car.SetAngles(sin(t.t * 9.0) * 2.0, t.yaw, sin(t.t * 13.0) * 1.5)
			keep.append(t)
		} else {
			if (!t.hit) { t.hit = true; t.out = 0.0 }
			t.out += dt
			local k = t.out / 1.6
			if (k >= 1.0) { t.car.Destroy(); continue }
			// crash: slides on, spins, hops
			local dir = t.to - t.from
			dir.z = 0.0
			dir.Norm()
			local p = t.to + dir * (70.0 * k)
			p.z = p.z + fabs(sin(k * 9.0)) * 18.0 * (1.0 - k)
			t.car.SetOrigin(p)
			t.car.SetAngles(0.0, t.yaw + 220.0 * k, 12.0 * k)
			keep.append(t)
		}
	}
	::GTA.taxis = keep
	// wrecked police cars
	local wk = []
	foreach (w in ::GTA.wrecks) {
		if (!w.ent.IsValid()) continue
		w.t += dt
		if (w.t >= w.life) { w.ent.Destroy(); continue }
		local z = 150.0 * w.t - 260.0 * w.t * w.t / 2.0
		if (z < 0.0) z = 0.0
		w.ent.SetOrigin(w.pos + Vector(0.0, 0.0, z))
		local rot = w.t < 1.1 ? w.t * 330.0 * w.spin : 360.0 * w.spin
		w.ent.SetAngles(rot, w.yaw, 8.0 * w.spin)
		wk.append(w)
	}
	::GTA.wrecks = wk
	// cash stacks fall and spin, then lie on the floor for a moment
	local ck = []
	foreach (s in ::GTA.cash_stacks) {
		if (!s.ent.IsValid()) continue
		if (s.landed > 0.0) {
			s.landed += dt
			if (s.landed > 2.4) { s.ent.Destroy(); continue }
			ck.append(s)
			continue
		}
		s.t += dt
		s.z -= s.v * dt
		s.yaw += s.spin * dt
		if (s.z <= s.floor) { s.z = s.floor; s.landed = dt; s.ent.SetAngles(0.0, s.yaw, 0.0) }
		else s.ent.SetAngles(s.tilt, s.yaw, s.tilt * 0.5 + s.t * 90.0)
		s.ent.SetOrigin(Vector(s.x, s.y, s.z))
		ck.append(s)
	}
	::GTA.cash_stacks = ck
}

// ---------- events ----------

::GtaWasted <- function() {
	::GTA.busy = true
	::GtaFade("15 15 15", 90, 0.1, 0.35, 0)
	::GTA.bannerEnt = "wasted"
	::GTA.bannerUntil = Time() + 0.34
	SigfSound("sigf/wasted.wav")
	SigfCmd("host_timescale 0.35")
	SigfIn(0.4, function() {
		SigfCmd("host_timescale 1")
		::GTA.busy = false
	})
}

::GtaPassed <- function() {
	::GTA.busy = true
	::GTA.wanted = 0
	::GtaFade("255 210 60", 45, 0.3, 0.1, 0)
	::GTA.bannerEnt = "passed"
	::GTA.bannerUntil = Time() + 3.2
	SigfSound("sigf/cash.wav")
	::GtaRain(16)
	::GtaCash(5000)
	SigfIn(3.0, function() { ::GTA.busy = false })
}

// One chase: cops arrive, taxis ram them one by one, MISSION PASSED with cash rain.
// GtaRestart() starts it over (the demo calls it so the clip starts with the cops arriving).
::GtaRestart <- function() {
	::GTA.t0 = Time()
	::GTA.fired = {}
	::GTA.cash = 0
	::GTA.busy = false
	foreach (c in ::GTA.cops) {
		if (c.turret != null && c.turret.IsValid()) EntFireByHandle(c.turret, "Kill", "", 0.0, null, null)
		if (c.car != null && c.car.IsValid()) c.car.Destroy()
	}
	::GTA.cops = []
	::GTA.wave = 0
	::GTA.wanted = 0
	::GTA.logoUntil = Time() + 4.5
	SigfCmd("host_timescale 1")
}

::GtaDirector <- function() {
	local t = Time() - ::GTA.t0
	local f = ::GTA.fired
	if (t >= 0.0 && !("cops" in f)) { f.cops <- true; ::GtaCops() }
	local n = ::GtaCopCount()
	for (local i = 0; i < n; i++) {
		local tk = 8.0 + i * 2.6
		local k1 = "t" + i
		local k2 = "k" + i
		if (t >= tk - 1.5 && !(k1 in f)) { f[k1] <- true; ::GtaTaxi(i, 1.5) }
		if (t >= tk && !(k2 in f)) { f[k2] <- true; ::GtaKill(i) }
	}
	local tp = 8.0 + n * 2.6 + 0.8
	if (t >= tp && !("passed" in f)) { f.passed <- true; ::GtaPassed() }
	if (t >= tp + 6.0) {
		local w = ::GTA.wave
		local cash = ::GTA.cash
		::GtaRestart()
		::GTA.logoUntil = 0.0
		::GTA.wave = w
		::GTA.cash = cash
	}
}

::GtaGunfire <- function() {
	if (::GTA.wanted > 0 && !::GTA.busy && RandomInt(0, 2) > 0) SigfSound("sigf/gunshot.wav")
}

// the model pool is built while the view looks at the floor (the game only places models where the crosshair lands near)
SigfAfter(0.3, function() {
	local host = SigfHost()
	local best = 0.0
	local room = -1.0
	for (local i = 0; i < 12; i++) {
		local free = ::SigfFree(host.GetOrigin(), i * 30.0, 220.0)
		if (free > room + 0.001) { room = free; best = i * 30.0 }
	}
	::SigfLookRaw(35.0, best)
	::GtaBuildHud()
})
SigfAfter(5.0, function() {
	local f = SigfForward()
	::SigfLookRaw(::GTA.pitch, atan2(f.y, f.x) * 57.29578)
})
SigfAfter(5.0, function() {
	::GtaCash(0)
	SigfSound("sigf/radio.wav")
})
SigfEvery(30.5, function() { SigfSound("sigf/radio.wav") })
SigfEvery(0.9, function() { ::GtaGunfire() })
SigfEvery(0.25, function() { if (::GTA.hudReady) ::GtaDirector() })
SigfEvery(0.35, function() { ::GtaLights() })
SigfEvery(1.0, function() { ::GtaCash(0) })

// fast clock for the animation (the kit clock only ticks 10 times per second)
SigfAfter(0.1, function() {
	local t = Entities.CreateByClassname("logic_timer")
	t.__KeyValueFromString("targetname", "gta_fast_clock")
	t.ValidateScriptScope()
	t.GetScriptScope().GtaOnTimer <- function() { ::GtaFast() }
	t.ConnectOutput("OnTimer", "GtaOnTimer")
	EntFireByHandle(t, "RefireTime", "0.03", 0.0, null, null)
	EntFireByHandle(t, "Enable", "", 0.0, null, null)
})
