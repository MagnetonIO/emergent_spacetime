// Mobile navigation toggle
document.getElementById('navToggle').addEventListener('click', function() {
    document.getElementById('navMenu').classList.toggle('active');
});

// Smooth scrolling for navigation links
document.querySelectorAll('a[href^="#"]').forEach(anchor => {
    anchor.addEventListener('click', function (e) {
        e.preventDefault();
        const target = document.querySelector(this.getAttribute('href'));
        if (target) {
            target.scrollIntoView({
                behavior: 'smooth',
                block: 'start'
            });
            // Close mobile menu if open
            document.getElementById('navMenu').classList.remove('active');
        }
    });
});

// Quantum canvas animation
const canvas = document.getElementById('quantumCanvas');
const ctx = canvas?.getContext('2d');

if (canvas && ctx) {
    canvas.width = window.innerWidth;
    canvas.height = window.innerHeight;
    
    const particles = [];
    const particleCount = 100;
    const connections = [];
    const maxDistance = 150;
    
    class Particle {
        constructor() {
            this.x = Math.random() * canvas.width;
            this.y = Math.random() * canvas.height;
            this.vx = (Math.random() - 0.5) * 0.5;
            this.vy = (Math.random() - 0.5) * 0.5;
            this.radius = Math.random() * 2 + 1;
            this.opacity = Math.random() * 0.5 + 0.5;
        }
        
        update() {
            this.x += this.vx;
            this.y += this.vy;
            
            if (this.x < 0 || this.x > canvas.width) this.vx *= -1;
            if (this.y < 0 || this.y > canvas.height) this.vy *= -1;
        }
        
        draw() {
            ctx.beginPath();
            ctx.arc(this.x, this.y, this.radius, 0, Math.PI * 2);
            ctx.fillStyle = `rgba(99, 102, 241, ${this.opacity})`;
            ctx.fill();
        }
    }
    
    function init() {
        for (let i = 0; i < particleCount; i++) {
            particles.push(new Particle());
        }
    }
    
    function drawConnections() {
        for (let i = 0; i < particles.length; i++) {
            for (let j = i + 1; j < particles.length; j++) {
                const dx = particles[i].x - particles[j].x;
                const dy = particles[i].y - particles[j].y;
                const distance = Math.sqrt(dx * dx + dy * dy);
                
                if (distance < maxDistance) {
                    const opacity = 1 - distance / maxDistance;
                    ctx.beginPath();
                    ctx.moveTo(particles[i].x, particles[i].y);
                    ctx.lineTo(particles[j].x, particles[j].y);
                    ctx.strokeStyle = `rgba(139, 92, 246, ${opacity * 0.3})`;
                    ctx.lineWidth = 0.5;
                    ctx.stroke();
                }
            }
        }
    }
    
    function animate() {
        ctx.clearRect(0, 0, canvas.width, canvas.height);
        
        particles.forEach(particle => {
            particle.update();
            particle.draw();
        });
        
        drawConnections();
        requestAnimationFrame(animate);
    }
    
    init();
    animate();
    
    // Resize canvas on window resize
    window.addEventListener('resize', () => {
        canvas.width = window.innerWidth;
        canvas.height = window.innerHeight;
    });
}

// E = Ic² calculator
function calculateEnergy() {
    const infoInput = document.getElementById('infoInput');
    const infoDensity = parseFloat(infoInput.value);
    const c = 299792458; // Speed of light in m/s
    const c2 = c * c;
    
    const energy = infoDensity * c2;
    const mass = energy / c2;
    
    document.getElementById('energyValue').textContent = energy.toExponential(2) + ' J';
    document.getElementById('massValue').textContent = mass.toExponential(2) + ' kg';
}

// Quantum circuit visualization
function runQuantumSimulation() {
    const canvas = document.getElementById('circuitCanvas');
    const ctx = canvas?.getContext('2d');
    
    if (!canvas || !ctx) return;
    
    // Clear canvas
    ctx.fillStyle = '#0f0f23';
    ctx.fillRect(0, 0, canvas.width, canvas.height);
    
    // Draw quantum circuit gates
    const gates = ['H', 'X', 'CNOT', 'Z', 'H'];
    const qubits = 3;
    const gateWidth = 40;
    const gateHeight = 30;
    const spacing = 60;
    
    // Draw qubit lines
    ctx.strokeStyle = '#6366f1';
    ctx.lineWidth = 2;
    for (let i = 0; i < qubits; i++) {
        const y = 50 + i * 50;
        ctx.beginPath();
        ctx.moveTo(20, y);
        ctx.lineTo(canvas.width - 20, y);
        ctx.stroke();
    }
    
    // Draw gates
    gates.forEach((gate, index) => {
        const x = 50 + index * spacing;
        const qubit = Math.floor(Math.random() * qubits);
        const y = 50 + qubit * 50;
        
        // Gate background
        ctx.fillStyle = '#8b5cf6';
        ctx.fillRect(x - gateWidth/2, y - gateHeight/2, gateWidth, gateHeight);
        
        // Gate text
        ctx.fillStyle = '#ffffff';
        ctx.font = '14px JetBrains Mono';
        ctx.textAlign = 'center';
        ctx.textBaseline = 'middle';
        ctx.fillText(gate, x, y);
        
        // CNOT connections
        if (gate === 'CNOT' && qubit < qubits - 1) {
            ctx.beginPath();
            ctx.arc(x, y + 50, 5, 0, Math.PI * 2);
            ctx.fill();
            ctx.beginPath();
            ctx.moveTo(x, y + gateHeight/2);
            ctx.lineTo(x, y + 45);
            ctx.stroke();
        }
    });
    
    // Animate measurement
    ctx.fillStyle = '#10b981';
    ctx.font = '12px Inter';
    ctx.textAlign = 'left';
    ctx.fillText('|ψ⟩ = α|000⟩ + β|111⟩', canvas.width - 150, 30);
}

// Initialize MathJax
window.MathJax = {
    tex: {
        inlineMath: [['$', '$'], ['\\(', '\\)']],
        displayMath: [['$$', '$$'], ['\\[', '\\]']],
    },
    svg: {
        fontCache: 'global'
    }
};

// Intersection Observer for fade-in animations
const observerOptions = {
    root: null,
    rootMargin: '0px',
    threshold: 0.1
};

const observer = new IntersectionObserver((entries) => {
    entries.forEach(entry => {
        if (entry.isIntersecting) {
            entry.target.style.opacity = '1';
            entry.target.style.transform = 'translateY(0)';
        }
    });
}, observerOptions);

// Observe all cards for animation
document.addEventListener('DOMContentLoaded', () => {
    const cards = document.querySelectorAll('.theory-card, .impl-card, .demo-card, .paper-card');
    cards.forEach(card => {
        card.style.opacity = '0';
        card.style.transform = 'translateY(20px)';
        card.style.transition = 'opacity 0.6s, transform 0.6s';
        observer.observe(card);
    });
});

// Add parallax effect to hero section
window.addEventListener('scroll', () => {
    const scrolled = window.pageYOffset;
    const hero = document.querySelector('.hero-content');
    if (hero) {
        hero.style.transform = `translateY(${scrolled * 0.5}px)`;
    }
});

// Initialize on page load
document.addEventListener('DOMContentLoaded', () => {
    calculateEnergy();
    runQuantumSimulation();
});