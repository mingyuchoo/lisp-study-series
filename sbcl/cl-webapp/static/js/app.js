// Main JavaScript file for the Common Lisp Web Application

document.addEventListener('DOMContentLoaded', function() {
  console.log('Common Lisp Web Application initialized');
  
  // Add click event to all feature cards for highlighting
  const featureCards = document.querySelectorAll('.feature-card');
  featureCards.forEach(card => {
    card.addEventListener('click', function() {
      this.classList.toggle('highlighted');
    });
  });
  
  // Add active class to current navigation item
  const currentPath = window.location.pathname;
  const navLinks = document.querySelectorAll('nav a');
  
  navLinks.forEach(link => {
    const linkPath = link.getAttribute('href');
    if (currentPath === linkPath || 
        (linkPath !== '/' && currentPath.startsWith(linkPath))) {
      link.classList.add('active');
    }
  });
  
  // Add some dynamic styles
  const style = document.createElement('style');
  style.textContent = `
    .feature-card.highlighted {
      border-left: 4px solid #4a86e8;
      transform: translateX(5px);
      transition: all 0.3s ease;
    }
    
    nav a.active {
      font-weight: bold;
      text-decoration: underline;
    }
  `;
  document.head.appendChild(style);
});
