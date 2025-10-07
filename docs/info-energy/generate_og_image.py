#!/usr/bin/env python3
"""
Generate Open Graph image for E = Ic² website
Creates a 1200x630px image for Facebook/social media sharing
"""

from PIL import Image, ImageDraw, ImageFont
import os

def create_og_image():
    # Create image with gradient background
    width, height = 1200, 630
    img = Image.new('RGB', (width, height), '#0f0f23')
    draw = ImageDraw.Draw(img)
    
    # Create gradient background
    for y in range(height):
        # Gradient from dark blue to slightly lighter
        ratio = y / height
        r = int(15 + (26 - 15) * ratio)  # 0x0f to 0x1a
        g = int(15 + (26 - 15) * ratio)
        b = int(35 + (46 - 35) * ratio)  # 0x23 to 0x2e
        draw.rectangle([(0, y), (width, y + 1)], fill=(r, g, b))
    
    # Add grid pattern overlay
    for x in range(0, width, 50):
        draw.line([(x, 0), (x, height)], fill=(99, 102, 241, 20), width=1)
    for y in range(0, height, 50):
        draw.line([(0, y), (width, y)], fill=(139, 92, 246, 20), width=1)
    
    # Try to use system fonts
    try:
        # Try different font paths based on OS
        font_paths = [
            '/System/Library/Fonts/Helvetica.ttc',  # macOS
            '/usr/share/fonts/truetype/liberation/LiberationSans-Bold.ttf',  # Linux
            'C:\\Windows\\Fonts\\arial.ttf',  # Windows
        ]
        
        title_font = None
        equation_font = None
        subtitle_font = None
        
        for font_path in font_paths:
            if os.path.exists(font_path):
                equation_font = ImageFont.truetype(font_path, 100)
                title_font = ImageFont.truetype(font_path, 36)
                subtitle_font = ImageFont.truetype(font_path, 22)
                break
        
        if not equation_font:
            # Fallback to default font
            equation_font = ImageFont.load_default()
            title_font = ImageFont.load_default()
            subtitle_font = ImageFont.load_default()
            
    except:
        equation_font = ImageFont.load_default()
        title_font = ImageFont.load_default()
        subtitle_font = ImageFont.load_default()
    
    # Draw quantum nodes
    node_positions = [
        (200, 100), (400, 150), (600, 200), (800, 250), (1000, 300),
        (300, 400), (500, 450), (700, 500), (900, 550)
    ]
    
    for x, y in node_positions:
        # Draw glow
        for i in range(20, 0, -2):
            alpha = int(255 * (1 - i/20) * 0.3)
            draw.ellipse([(x-i, y-i), (x+i, y+i)], 
                        fill=(99, 102, 241, alpha))
        # Draw node
        draw.ellipse([(x-4, y-4), (x+4, y+4)], fill=(99, 102, 241))
    
    # Draw connections between nodes
    for i in range(len(node_positions) - 1):
        x1, y1 = node_positions[i]
        x2, y2 = node_positions[i + 1]
        draw.line([(x1, y1), (x2, y2)], fill=(139, 92, 246, 80), width=1)
    
    # Draw corner decorations
    corner_size = 100
    corner_color = (99, 102, 241, 100)
    # Top left
    draw.line([(30, 30), (30 + corner_size, 30)], fill=corner_color, width=3)
    draw.line([(30, 30), (30, 30 + corner_size)], fill=corner_color, width=3)
    # Top right
    draw.line([(width - 30 - corner_size, 30), (width - 30, 30)], fill=corner_color, width=3)
    draw.line([(width - 30, 30), (width - 30, 30 + corner_size)], fill=corner_color, width=3)
    # Bottom left
    draw.line([(30, height - 30), (30 + corner_size, height - 30)], fill=corner_color, width=3)
    draw.line([(30, height - 30 - corner_size), (30, height - 30)], fill=corner_color, width=3)
    # Bottom right
    draw.line([(width - 30 - corner_size, height - 30), (width - 30, height - 30)], fill=corner_color, width=3)
    draw.line([(width - 30, height - 30 - corner_size), (width - 30, height - 30)], fill=corner_color, width=3)
    
    # Draw main equation
    equation_text = "E = Ic²"
    equation_color = (102, 126, 234)  # Purple-blue gradient effect
    
    # Calculate text positions
    eq_bbox = draw.textbbox((0, 0), equation_text, font=equation_font)
    eq_width = eq_bbox[2] - eq_bbox[0]
    eq_x = (width - eq_width) // 2
    eq_y = 200
    
    # Draw equation with glow effect
    for offset in range(3, 0, -1):
        draw.text((eq_x - offset, eq_y), equation_text, 
                 fill=(102, 126, 234, 100), font=equation_font)
        draw.text((eq_x + offset, eq_y), equation_text, 
                 fill=(102, 126, 234, 100), font=equation_font)
    draw.text((eq_x, eq_y), equation_text, fill=(150, 170, 255), font=equation_font)
    
    # Draw title
    title_text = "Information-Energy Equivalence"
    title_bbox = draw.textbbox((0, 0), title_text, font=title_font)
    title_width = title_bbox[2] - title_bbox[0]
    draw.text(((width - title_width) // 2, 340), title_text, 
             fill=(226, 232, 240), font=title_font)
    
    # Draw subtitle (split into lines)
    subtitle_lines = [
        "A fundamental reconceptualization where spacetime, matter, and energy",
        "emerge from quantum information processing"
    ]
    
    y_offset = 400
    for line in subtitle_lines:
        line_bbox = draw.textbbox((0, 0), line, font=subtitle_font)
        line_width = line_bbox[2] - line_bbox[0]
        draw.text(((width - line_width) // 2, y_offset), line, 
                 fill=(148, 163, 184), font=subtitle_font)
        y_offset += 30
    
    # Draw badges
    badges = [
        ("⚛️ Quantum Theory", 350),
        ("🔬 Emergent Spacetime", 550),
        ("📐 Category Theory", 750)
    ]
    
    badge_y = 480
    for badge_text, badge_x in badges:
        # Draw badge background
        draw.rounded_rectangle([(badge_x - 80, badge_y), (badge_x + 80, badge_y + 40)],
                               radius=8, fill=(99, 102, 241, 40), 
                               outline=(99, 102, 241, 100), width=2)
        # Draw badge text
        draw.text((badge_x - 70, badge_y + 10), badge_text, 
                 fill=(165, 180, 252), font=subtitle_font)
    
    # Draw URL at bottom
    url_text = "magnetonio.github.io/emergent_spacetime"
    url_bbox = draw.textbbox((0, 0), url_text, font=subtitle_font)
    url_width = url_bbox[2] - url_bbox[0]
    draw.text(((width - url_width) // 2, height - 50), url_text, 
             fill=(100, 116, 139), font=subtitle_font)
    
    # Save image
    img.save('og-image.png', 'PNG', quality=95)
    print("OG image generated successfully: og-image.png")
    print(f"Image dimensions: {width}x{height}")
    print("Ready for Facebook/social media sharing!")

if __name__ == "__main__":
    create_og_image()