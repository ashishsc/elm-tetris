import './main.css';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

const tetris = Main.embed(document.getElementById('root'));

registerServiceWorker();

// Ports
tetris.ports.toggleMusic.subscribe(musicEnabled => {
    const player = document.getElementById('music');
    if (musicEnabled) {
        player.pause();
        player.muted = true;
    } else {
        player.muted = false;
        player.play();
    }
});