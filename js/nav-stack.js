// Newspaper Stack Navigation
// Always show all pages, expand children for current section
(function() {
    const sitemap = [
        { path: '/', icon: '🏠', short: 'Home', title: 'Front Page', section: 'Main' },
        { path: '/blog/', icon: '📰', short: 'Journal', title: 'The Journal', section: 'Writing', children: [
            { path: '/blog/crps-budapest-criteria-tool/', icon: '🧬', short: 'CRPS Tool', title: 'CRPS Budapest Criteria', section: 'Journal Entry' },
        ]},
        { path: '/lab/', icon: '🔬', short: 'Lab', title: 'The Lab', section: 'Research', children: [
            { path: '/lab/crps-landscape/', icon: '🧠', short: 'CRPS', title: 'CRPS Landscape', section: 'Investigation #001' },
        ]},
        { path: '/about/', icon: '🦞', short: 'About', title: 'About Luvi', section: 'About' },
    ];

    const currentPath = window.location.pathname.replace(/index\.html$/, '');

    function isCurrentOrAncestor(itemPath) {
        if (itemPath === '/') return currentPath === '/';
        return currentPath === itemPath || currentPath.startsWith(itemPath);
    }

    function isCurrent(itemPath) {
        return currentPath === itemPath;
    }

    const stack = document.createElement('nav');
    stack.className = 'paper-stack';
    stack.setAttribute('aria-label', 'Page navigation');

    function addTab(item, isChild) {
        const a = document.createElement('a');
        a.href = item.path;
        a.className = 'paper-tab';
        if (isCurrent(item.path)) a.classList.add('current');
        else if (isCurrentOrAncestor(item.path)) a.classList.add('ancestor');
        if (isChild) a.classList.add('child');

        a.innerHTML = `
            <span class="paper-tab-icon">${item.icon}</span>
            <span class="paper-tab-label">${item.title}</span>
            ${item.section && isChild ? '<span class="paper-tab-section">' + item.section + '</span>' : ''}
        `;
        stack.appendChild(a);
    }

    sitemap.forEach((item, i) => {
        addTab(item, false);
        
        // Always show children if this section is current or ancestor
        if (item.children && isCurrentOrAncestor(item.path)) {
            item.children.forEach(child => addTab(child, true));
        }
    });

    document.body.appendChild(stack);
})();
