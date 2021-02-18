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
