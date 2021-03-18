var Hyphenopoly = {
	require: {
	    "it": "Supercalifragilistichespiralidoso",
	    "en-us": "Supercalifragilisticexpialidocious"
	},
	setup: {
	    selectors: {
		"article": {}
	    }
	}
    };

document.addEventListener('DOMContentLoaded', (event) => {
    manageDarkMode();
    manageLightBox();
    manageHyphenopoly();
})

function manageDarkMode() {
    let styles = ['theme-light', 'theme-dark', 'theme-yellow'];
    let switcher = document.getElementById('theme-switcher');
    let body = document.body;
    let active;
    if ((active = window.localStorage.getItem('active-theme')) === null)
	active = 0; //default theme
    
    function update() {
	for (let styleName of styles)
	    body.classList.remove(styleName);
	body.classList.add(styles[active]);
	//switcher.innerText = styles[(active + styles.length + 1) % styles.length];
    }

    update();
    
    switcher.onclick = function () {
        active = (active + 1) % styles.length;
	window.localStorage.setItem('active-theme', active);
        update();
    }
}

function manageLightBox() {
    modal = document.createElement('div');
    modal.id = 'lightbox';
    modal.onclick = function () {
	modal.style.display = 'none';
	modal.innerHTML = '';
	document.body.classList.toggle('noscroll');
    };
    document.body.appendChild(modal);
    
    for (let img of document.getElementsByTagName('img')) {
	img.setAttribute('data-original', 'false');
	img.onclick = function () {
	    if (img.getAttribute('data-original') == 'false') { 
		img.src = img.src.split('.')[0] + '~original.jpg';
		img.setAttribute('data-original', 'true');
	    }
	    modal.style.display = 'block';
	    modal.appendChild(img.cloneNode());
	    document.body.classList.toggle('noscroll');
	};
    }
}

function manageHyphenopoly() {
}
