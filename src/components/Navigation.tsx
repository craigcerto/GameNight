"use client"

import Link from 'next/link'
import { usePathname } from 'next/navigation'
import { Home, Play, Users, Trophy } from 'lucide-react'
import { cn } from '@/lib/utils'

const navItems = [
  { href: '/', label: 'Home', icon: Home },
  { href: '/play', label: 'Play', icon: Play },
  { href: '/players', label: 'Players', icon: Users },
]

export function Navigation() {
  const pathname = usePathname()

  return (
    <nav className="sticky top-0 z-50 w-full border-b border-neon-pink/20 bg-background/80 backdrop-blur-lg">
      <div className="container flex h-16 items-center">
        {/* Logo */}
        <Link href="/" className="mr-8 flex items-center space-x-2 group">
          <Trophy className="h-6 w-6 text-neon-pink transition-all duration-300 group-hover:drop-shadow-[0_0_8px_#ff2d75]" />
          <span className="font-display text-xl font-bold bg-gradient-to-r from-neon-pink to-neon-cyan bg-clip-text text-transparent transition-all duration-300 group-hover:drop-shadow-[0_0_8px_#ff2d75]">
            GameNight
          </span>
        </Link>

        {/* Nav Links */}
        <div className="flex items-center space-x-1">
          {navItems.map((item) => {
            const Icon = item.icon
            const isActive = pathname === item.href ||
              (item.href !== '/' && pathname.startsWith(item.href))

            return (
              <Link
                key={item.href}
                href={item.href}
                className={cn(
                  "flex items-center space-x-2 px-4 py-2 rounded-lg text-sm font-medium transition-all duration-300",
                  isActive
                    ? "text-neon-pink bg-neon-pink/10 shadow-[0_0_10px_rgba(255,45,117,0.3)]"
                    : "text-muted-foreground hover:text-neon-cyan hover:bg-neon-cyan/5"
                )}
              >
                <Icon className="h-4 w-4" />
                <span>{item.label}</span>
              </Link>
            )
          })}
        </div>
      </div>
    </nav>
  )
}
