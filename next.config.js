/** @type {import('next').NextConfig} */
const nextConfig = {
  images: {
    remotePatterns: [
      {
        protocol: 'https',
        hostname: '*.supabase.co',
        port: '',
        pathname: '/storage/v1/object/public/**',
      },
    ],
  },
  experimental: {
    // Exclude problematic patterns from build trace to avoid stack overflow
    outputFileTracingExcludes: {
      '*': ['**/*'],
    },
  },
  // Increase Node.js stack size during build
  webpack: (config, { isServer }) => {
    if (isServer) {
      config.optimization = {
        ...config.optimization,
        moduleIds: 'named',
      }
    }
    return config
  },
}

module.exports = nextConfig
