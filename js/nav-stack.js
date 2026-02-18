// Paper Stack Navigation
// Pages as physical papers in a messy desk stack
(function () {
    var sitemap = [
        { path: '/',           icon: '🏠', title: 'Home' },
        { path: '/lab/',       icon: '🔬', title: 'The Lab', children: [
            { path: '/lab/exp-001/', icon: '①', title: 'EXP-001: Cost of Linearity' },
            { path: '/lab/exp-002/', icon: '②', title: 'EXP-002: Oracle Haircut' },
            { path: '/lab/exp-003/', icon: '③', title: 'EXP-003: ANCOVA Bias' },
            { path: '/lab/exp-004/', icon: '④', title: 'EXP-004: K-Selection' },
            { path: '/lab/exp-005/', icon: '⑤', title: 'EXP-005: Stress Test' },
            { path: '/lab/exp-006/', icon: '⑥', title: 'EXP-006: Permutation' },
        ]},
        { path: '/boardroom/', icon: '🏛️', title: 'Board Room', children: [
            { path: '/boardroom/sessions/session-001/', icon: '①', title: 'Session 001' },
            { path: '/boardroom/sessions/session-002/', icon: '②', title: 'Session 002' },
            { path: '/boardroom/sessions/session-003/', icon: '③', title: 'Session 003' },
            { path: '/boardroom/sessions/session-004/', icon: '④', title: 'Session 004' },
            { path: '/boardroom/sessions/session-005/', icon: '⑤', title: 'Session 005' },
            { path: '/boardroom/sessions/session-006/', icon: '⑥', title: 'Session 006' },
        ]},
        { path: '/preprint/',  icon: '📄', title: 'Preprint' },
        { path: '/how/',       icon: '⚙️', title: 'How It Works' },
        { path: '/blog/',      icon: '📰', title: 'Journal' },
        { path: '/about/',     icon: '🦞', title: 'About' },
    ]

    var cur = window.location.pathname.replace(/index\.html$/, '')

    function isCurrent (p) {
        return cur === p
    }
    function isAncestor (p) {
        if (p === '/') return cur === '/'
        return cur.indexOf(p) === 0
    }

    var nav = document.createElement('nav')
    nav.className = 'paper-stack'
    nav.setAttribute('aria-label', 'Site navigation')

    function addPage (item, isChild) {
        var a = document.createElement('a')
        a.href = item.path
        a.className = 'paper-page'
        if (isCurrent(item.path)) a.classList.add('current')
        else if (isAncestor(item.path)) a.classList.add('ancestor')
        if (isChild) a.classList.add('child')

        a.innerHTML =
            '<span class="paper-page-icon">' + item.icon + '</span>' +
            '<span class="paper-page-title">' + item.title + '</span>'

        nav.appendChild(a)
    }

    sitemap.forEach(function (item) {
        addPage(item, false)
        if (item.children && isAncestor(item.path)) {
            item.children.forEach(function (child) {
                addPage(child, true)
            })
        }
    })

    document.body.appendChild(nav)
})()
