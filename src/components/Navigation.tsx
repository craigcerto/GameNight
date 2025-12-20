"use client"

import Link from 'next/link'
import { usePathname } from 'next/navigation'
import { Home, Play, Users, Gamepad2 } from 'lucide-react'
import { cn } from '@/lib/utils'

const navItems = [
  { href: '/', label: 'Home', icon: Home },
  { href: '/play', label: 'Play', icon: Play },
  { href: '/players', label: 'Players', icon: Users },
]

export function Navigation() {
  const pathname = usePathname()

  return (
    <nav className="sticky top-0 z-50 w-full nav-arcade">
      <div className="container flex h-16 items-center justify-between">
        {/* Logo */}
        <Link href="/" className="flex items-center gap-3 group">
          <div className="relative">
            <Gamepad2 className="h-8 w-8 text-[#ff2d75] transition-all duration-300 group-hover:scale-110" />
            <div className="absolute inset-0 blur-lg bg-[#ff2d75]/30 group-hover:bg-[#ff2d75]/50 transition-all duration-300" />
          </div>
          <span className="font-display text-2xl font-bold gradient-text tracking-tight">
            GameNight
          </span>
        </Link>

        {/* Nav Links */}
        <div className="flex items-center gap-1">
          {navItems.map((item) => {
            const Icon = item.icon
            const isActive = pathname === item.href ||
              (item.href !== '/' && pathname.startsWith(item.href))

            return (
              <Link
                key={item.href}
                href={item.href}
                className={cn(
                  "flex items-center gap-2 px-4 py-2 rounded-xl text-sm font-medium transition-all duration-300",
                  isActive
                    ? "bg-[#ff2d75]/15 text-[#ff2d75] shadow-[0_0_20px_rgba(255,45,117,0.2)]"
                    : "text-white/60 hover:text-white hover:bg-white/5"
                )}
              >
                <Icon className={cn(
                  "h-4 w-4 transition-all duration-300",
                  isActive && "drop-shadow-[0_0_8px_rgba(255,45,117,0.8)]"
                )} />
                <span>{item.label}</span>
              </Link>
            )
          })}
        </div>
      </div>
    </nav>
  )
}
